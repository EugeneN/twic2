{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Components.Feed where

import           Control.Applicative      ((<$>), (<*>), (<|>))
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forM_, join, void, when)
import           Control.Monad.IO.Class   (liftIO)
import           Prelude

import           Data.Foldable            (asum)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List                as DL
import           Data.Maybe               (Maybe (..), catMaybes, fromMaybe,
                                           isJust, isNothing, listToMaybe)
import           Data.Monoid
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TLB

import qualified Network.URI              as NU

import qualified Reflex                   as R
import qualified Reflex.Host.App          as RHA

import qualified Data.JSString            as JSS
import qualified Data.JSString.RegExp     as RegExp
import qualified Data.VirtualDOM          as VD
import qualified JavaScript.Web.WebSocket as WS

import qualified HTMLEntities.Decoder     as HE

import           BL.Instances
import qualified BL.Types                 as BL

import           Components.Busy
import           Lib.FRP
import           Lib.FW
import           Lib.Net                  (getAPI)
import           Lib.UI
import           Types
import           UIConfig


allButLast n [] = []
allButLast n xs = DL.init xs

last_ n [] = []
last_ n xs = [DL.last xs]

data FeedAction = AddNew BL.Tweet | ShowNew | ShowOld Int
                | Search | WriteNew
                | MarkRt BL.Tweet Bool | MarkLv BL.Tweet Bool deriving (Show, Eq)
data TweetAction = Retweet BL.Tweet Bool | Reply BL.Tweet | Love BL.Tweet Bool | Go BL.Tweet
                 | UserInfo BL.Author | UserFeed BL.Author deriving (Show, Eq)
-- type Feed = ([BL.Tweet], [BL.Tweet], [BL.Tweet], Maybe FeedAction)
type Feed = (Set.Set BL.Tweet, Set.Set BL.Tweet, Set.Set BL.Tweet, Maybe FeedAction)

data ThreadElem = T BL.Tweet | Separator


dummyTweet i = BL.Tweet { BL.text                       = []
                        , BL.created_at                = ""
                        , BL.id                        = i
                        , BL.id_str                    = show i
                        , BL.user                      = a
                        , BL.entities                  = es
                        , BL.extendedEntities          = es
                        , BL.retweet                   = Nothing
                        , BL.status_favorited          = Nothing
                        , BL.status_retweeted          = Nothing
                        , BL.statusInReplyToStatusId   = Nothing
                        , BL.statusInReplyToUserId     = Nothing
                        , BL.statusInReplyToScreenName = Nothing }
  where
    a =  BL.Author { BL.name                  = ""
                   , BL.authorId              = 0
                   , BL.screen_name           = ""
                   , BL.default_profile_image = False
                   , BL.profile_image_url     = "" }
    es = BL.Entities { BL.urls     = []
                     , BL.hashtags = []
                     , BL.media    = Nothing }

mkTweetUrl t = "https://twitter.com/" <> (T.unpack . BL.screen_name . BL.user $ t) <> "/status/" <> show (BL.id t)
mkTweetUrl' tid sn = "https://twitter.com/" <> (fromMaybe "xxx" sn) <> "/status/" <> show tid

feedComponent :: R.Event t ChildAction
              -> (WSInterface t, R.Event t (Maybe WS.WebSocket))
              -> Sink UserInfoQuery
              -> Sink Notification
              -> Sink BusyCmd
              -> TheApp t m l Counter
feedComponent parentControllerE (wsi, wsReady) requestUserInfoU ntU busyU = do
  (tweetActionE :: R.Event t TweetAction, tweetActionU) <- RHA.newExternalEvent
  (controllerE :: R.Event t FeedAction, controllerU) <- RHA.newExternalEvent
  (modelE :: R.Event t (Either String WSData), modelU) <- RHA.newExternalEvent
  (tweetsE :: R.Event t BL.Tweet, tweetsU) <- RHA.newExternalEvent

  feedD  <- R.foldDyn feedOp (Set.empty,Set.empty,Set.empty, Nothing) $ R.ffilter isFeedOp controllerE
  -- modelD <- R.foldDyn (\x xs -> xs <> [x]) [] modelE

  (adhocCmdE :: R.Event t BL.TweetId, adhocCmdU) <- RHA.newExternalEvent
  (adhocE :: R.Event t BL.FeedMessage, adhocU) <- RHA.newExternalEvent

  let adhocE' = fmap filterSelfLinks $ R.fmapMaybe unpackTweets adhocE
  adhocD <- R.foldDyn (\x xs -> HM.insert (BL.id x) x xs) HM.empty adhocE'

  let allTweetsD = (,) <$> feedD <*> adhocD

  subscribeToEvent adhocE' (handleAdhocEvents adhocCmdU)
  subscribeToEvent' adhocCmdE $ loadAdhocTweet allTweetsD adhocU
  -- ws_rcve wsi ~> (print . mappend "Received from WS: " . show)
  ws_rcve wsi ~> modelU
  -- subscribeToEvent (R.ffilter (not . isFeedOp) controllerE) print
  subscribeToEvent modelE (handleModelEvents tweetsU)
  subscribeToEvent tweetsE (handleNewTweets controllerU adhocCmdU)
  subscribeToEvent (R.updated feedD) setTitle'
  subscribeToEvent tweetActionE (handleTweetActions controllerU requestUserInfoU)

  let ownViewDyn = fmap (render controllerU requestUserInfoU tweetActionU) allTweetsD

  return (ownViewDyn, pure (Counter 0))

  where
    handleAdhocEvents adhocCmdU x = do
      preloadEntities adhocCmdU x
      preloadThreads adhocCmdU x

    handleModelEvents tweetsU x = case x of
      Right (WSData xs) -> forM_ xs $ \y -> when (isTweet y) $ tweetsU (unpackTweet y) >> pure ()
      _ -> pure ()

    handleNewTweets controllerU adhocCmdU x' = do
      let x = filterSelfLinks x'
      controllerU (AddNew x)
      preloadEntities adhocCmdU x
      preloadThreads adhocCmdU x
      pure ()

    handleTweetActions controllerU requestUserInfoU c = forkIO . void $ case c of
      Retweet t b -> do
        -- optimistically patch tweet
        controllerU $ MarkRt t b

        let url = (if b then "/retweet/?id=" else "/unretweet/?id=") <> show (BL.id t)

        let go count url =
              if count == 0
              then do
                print "Retweet attempts count exceeded, giving up."
                controllerU $ MarkRt t (not b)
                ntU $ Error (if b then "Retweet definitely failed" else "Unretweet definitely failed") "👀"

              else do
                x :: Either String (BL.TheResponse) <- withBusy busyU . getAPI . JSS.pack $ url
                case x of
                  -- update tweet with actual result
                  Left e  -> do
                    print $ "(Un)Retweet failed, retrying in 100000ns; error was: " <> show e
                    threadDelay 100000
                    go (count - 1) url

                  Right (BL.Fail (BL.JsonApiError t' m)) -> do
                    print $ "(Un)Retweet failed, retrying  in 100000ns; error was: " <> (T.unpack t') <> (T.unpack m)
                    threadDelay 100000
                    go (count - 1) url

                  Right (BL.Ok (BL.JsonResponse _ fs))  -> pure True -- ntU $ Success "Retweeted!" (mkTweetLink_ fs)

        go 3 url

      Reply t -> do
        print "TODO reply component" >> pure False

      Go t -> do
        windowOpen $ JSS.pack $ mkTweetUrl t
        pure True

      Love t b -> do
        controllerU $ MarkLv t b
        let url = (if b then "/star/?id=" else "/unstar/?id=") <> show (BL.id t)

        let go count url =
              if count == 0
              then do
                print "Star attempts count exceeded, giving up."
                controllerU $ MarkLv t (not b)
                ntU $ Error (if b then "Star definitely failed" else "Unstar definitely failed") "👀"

              else do
                x :: Either String (BL.TheResponse) <- withBusy busyU . getAPI . JSS.pack $ url

                case x of
                  Left e -> do
                    print $ "(Un)Star failed, retrying in 100000ns; error was: " <> show e
                    threadDelay 100000
                    go (count - 1) url
                  Right (BL.Fail (BL.JsonApiError t' m)) -> do
                    print $ "(Un)Star failed, retrying  in 100000ns; error was: " <> (T.unpack t') <> (T.unpack m)
                    threadDelay 100000
                    go (count - 1) url
                  Right (BL.Ok (BL.JsonResponse _ fs))  -> pure True -- ntU $ Success "Loved the tweet!" (mkTweetLink_ fs)

        go 3 url

      UserInfo u -> do
        requestUserInfoU (RequestUserInfo . T.unpack $ BL.screen_name u)

      UserFeed u -> do
        x :: Either String (BL.TheResponse) <- withBusy busyU .
                                getAPI . JSS.pack $ "/userfeed/?sn=" <> show (BL.screen_name u)
        case x of
          Left e -> ntU $ Error ":-(" e
          Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
          Right (BL.Ok (BL.JsonResponse _ fs))  -> ntU $ Success "Loaded user feed!" "..."

    setTitle' (_,_,new,_) =
      setTitle $ case length new of
                    0 -> "No new tweets"
                    1 -> "1 new tweet"
                    x -> show x <> " new tweets"

    mkTweetLink_ fs =
      let t' = join $ fmap unpackTweets $ listToMaybe fs
      in maybe ":-)" mkTweetUrl t'

    toggleRt t x ts =
      let idx = Set.lookupIndex t ts
          ts' = case idx of
                Just idx' -> let z = Set.elemAt idx' ts
                                 as = Set.deleteAt idx' ts
                             in Set.insert (z{ BL.status_retweeted = Just x }) as
                Nothing -> ts
      in ts'
    toggleLv t x ts =
      let idx = Set.lookupIndex t ts
          ts' = case idx of
                Just idx' -> let z = Set.elemAt idx' ts
                                 as = Set.deleteAt idx' ts
                             in Set.insert (z{ BL.status_favorited = Just x }) as
                Nothing -> ts
      in ts'

    feedOp op (old, cur, new, _) = case op of
      AddNew t   -> (old, cur, (Set.insert t new), Just op)
      ShowNew    -> ((Set.union old cur), new, Set.empty, Just op)
      ShowOld n  -> let (t, old') = Set.deleteFindMax old in (old', (Set.insert t cur), new, Just op)
      MarkRt t x -> (toggleRt t x old, toggleRt t x cur, toggleRt t x new, Just op)
      MarkLv t x -> (toggleLv t x old, toggleLv t x cur, toggleLv t x new, Just op)
      _          -> (old, cur, new, Just op)

    isFeedOp x = case x of
      AddNew _   -> True
      ShowNew    -> True
      ShowOld _  -> True
      MarkRt _ _ -> True
      MarkLv _ _ -> True
      _          -> False

    filterSelfLinks t =
      let es = BL.entities t
          ls = BL.urls es
          newUrls = filter (go (BL.id t)) ls
      in t {BL.entities = (es {BL.urls = newUrls})}
      where
        go pid u =
          let m = RegExp.exec (JSS.pack $ BL.eExpandedUrl u) twitterPattern
          in case m of
            Just x -> let i = read . JSS.unpack . head . RegExp.subMatched $ x
                      in if i == pid then False else True

            Nothing -> True

    unpackTweets :: BL.FeedMessage -> Maybe BL.Tweet
    unpackTweets (BL.TweetMessage t) = Just t
    unpackTweets _ = Nothing

    unpackTweet (BL.TweetMessage t) = t

    isTweet (BL.TweetMessage _) = True
    isTweet _                   = False

    youtubePattern = RegExp.create
      (RegExp.REFlags { RegExp.multiline = True, RegExp.ignoreCase = True })
      "^(?:https?:)\\/\\/(?:www.)?youtu(?:.*\\/v\\/|.*v\\=|\\.be\\/)([A-Za-z0-9_\\-]{11})"

    instagramPattern = RegExp.create
      (RegExp.REFlags { RegExp.multiline = True, RegExp.ignoreCase = True })
      "^(?:https?:)\\/\\/(?:www.)?(?:instagram.com|instagr.am)\\/p\\/([A-Za-z0-9-_]+)"

    vimeoPattern = RegExp.create
      (RegExp.REFlags { RegExp.multiline = True, RegExp.ignoreCase = True })
      "^(?:https?:)\\/\\/(?:www\\.|player\\.)?vimeo.com\\/(?:channels\\/(?:\\w+\\/)?|groups\\/(?:[^\\/]*)\\/videos\\/|album\\/(?:\\d+)\\/video\\/|video\\/|)(\\d+)(?:[a-zA-Z0-9_\\-]+)?"

    twitterPattern = RegExp.create
      (RegExp.REFlags { RegExp.multiline = True, RegExp.ignoreCase = True })
      "^(?:https?:)\\/\\/twitter.com\\/(?:i\\/web|[A-Za-z0-9_]+)\\/status\\/([0-9]+)"

    matchEntity c p u =
      mapM_ (c . JSS.unpack . head . RegExp.subMatched)
            (RegExp.exec (JSS.pack $ BL.eExpandedUrl u) p)

    loadAdhocTweet allTweetsD adhocU tid = do
        ((old, cur, new, _), adhoc) <- R.sample $ R.current allTweetsD
        let z = dummyTweet tid
            a = z `Set.member` old
            b = z `Set.member` cur
            c = z `Set.member` new
            d = HM.member tid adhoc
            e = a || b || c || d

        case c of
          False -> liftIO $ void . forkIO $ go 3 tid
          True  -> pure ()

      where
        go count tid =
          if count == 0
          then
            print $ "Adhoc requests for parent thread count exceeded, tid=" <> show tid
          else do
            x :: Either String BL.TheResponse <- withBusy busyU .
                    getAPI . JSS.pack $ "/adhoc/?id=" <> show tid
            case x of
              Left e                                -> threadDelay 100000 >> go (count - 1) tid
              Right (BL.Fail (BL.JsonApiError t m)) -> threadDelay 100000 >> go (count - 1) tid
              Right (BL.Ok (BL.JsonResponse _ ts))  -> forM_ ts adhocU


    preloadThreads adhocCmdU x = case BL.statusInReplyToStatusId x of
      Just pid -> adhocCmdU pid >> pure ()
      Nothing -> pure ()

    preloadEntities adhocCmdU t = void . forkIO $
      forM_ (BL.urls . BL.entities $ t) $ matchEntity (go (BL.id t) adhocCmdU) twitterPattern

      where
        go pid adhocCmdU x = do
          let y = read x
          when (y /= pid) $ adhocCmdU y >> pure ()
          -- TODO skip loading if already cached

    render :: Sink FeedAction -> Sink UserInfoQuery -> Sink TweetAction -> (Feed, HM.HashMap BL.TweetId BL.Tweet) -> VD.VNode l
    render controllerU requestUserInfoU tweetActionU ((old, cur, new, cmd), adhoc) =
      block [statusBar, historyButton, tweetList (old, cur, new), refreshButton new]

      where
        statusBar =
          let o = Set.size old
              c = Set.size cur
              n = Set.size new
              a = HM.size adhoc
              total = o + c + n + a
          in block_ "status-bar-wrapper"
                    [ VD.text $ show o <> "/" <> show c <> "/" <> show n <> "/" <> show a <> "/" <> show total ]

        historyButton =
          VD.h "div"
            (VD.prop [("style", "text-align: center; margin-top: 15px;")])
            [ buttonIcon "" "send-o" "New tweet" (A [("id", "write-new-tweet-id"), ("class", "history-button")])
                           [ onClick_ $ controllerU WriteNew ]
            , buttonIcon "" "ellipsis-h" "Show older tweets" (A [("id", "load-history-tweets-id"), ("class", "history-button")])
                          [ onClick_ $ controllerU (ShowOld 1) ]
            , buttonIcon "" "search" "Search" (A [("id", "search-tweets-id"), ("class", "history-button")])
                          [ onClick_ $ controllerU Search ]
            ]

        groupByParent :: (Set.Set BL.Tweet, [[BL.Tweet]]) -> [[BL.Tweet]]
        groupByParent (x, acc) | Set.null x = acc
        groupByParent (s, acc) =
          let x = Set.findMax s
              rest = x `Set.delete` s

              ps = findParents rest x
              ps' = ps <> [x]
              rest' = rest Set.\\ (Set.fromList ps')
          in groupByParent (rest', acc <> [ps'])

        -- XXX inefficient algo; proof of concept only; TODO refactor later
        findParents :: Set.Set BL.Tweet -> BL.Tweet -> [BL.Tweet]
        findParents us t =
          let idx = fromMaybe Nothing . fmap ((flip Set.lookupIndex) us) . fmap dummyTweet . BL.statusInReplyToStatusId $ t
              p = case idx of
                    Just idx' -> [Set.elemAt idx' us]
                    Nothing -> []
              ps = join $ findParents us <$> p
          in ps <> p

        -- findChildren :: [BL.Tweet] -> BL.Tweet -> [BL.Tweet]
        -- findChildren us t =
        --   let p = filter (\t' -> BL.statusInReplyToStatusId t' == Just (BL.id t)) us
        --       ps = join $ findChildren us <$> p
        --   in p <> ps

        us = old `Set.union` cur `Set.union` new `Set.union` Set.fromList (HM.elems adhoc)
        grouped' = DL.reverse . groupByParent $ (us, [])
        grouped = filter (\xs -> DL.last xs `Set.member` cur) grouped'

        tweetList (old, cur, new) =
          let c = case cmd of
                    Just ShowNew -> container'
                    otherwise    -> container
          in c [list $ if Set.null cur then [noTweetsLabel "EOF"]
                                       else fmap (block_ "thread-block" . (: []) . thread) grouped]

        refreshButton new =
          VD.h "div"
            (VD.prop [("class", "refresh")])
            [button (show $ length new)
                    (p_ [("class", if null new then "no-new-tweets" else "there-are-new-tweets")])
                    [ onClick_ (controllerU ShowNew >> scrollToTop) ]
            ]

        thread ts =
          let wts = fmap T ts
              sts = DL.intersperse Separator wts
          in VD.h "div" (p_ [("style", "padding: 0px; border: 0px solid grey; width: auto; display: inline-block; margin: 0px;")])
                      $ fmap tweetOrSep sts

        tweetOrSep (T t) = tweet t
        tweetOrSep Separator = VD.h "div" (p_ [("style", "color: #aaa; padding: 0px; border: 0px solid grey; width: auto; display: inline-block; margin: 0px; padding-left: 25px;")])
                                          [VD.text "↓"]

        tweet t = panelRel $ [ toolbarRt t, toolbarRe t, toolbarLk t, toolbarGo t, author t, body (fromMaybe t (BL.retweet t)) ]
                           <> entities (BL.entities t)

        renderMediaImage m = VD.h "div" (p_ [("class", "media")])
                                        [link_ (T.pack $ BL.mMediaUrl m)
                                                 (VD.h "img"
                                                       (p_ [ ("class", "inline-img")
                                                           , ("src", BL.mMediaUrl m)
                                                           , ("title", "")] )
                                                       []
                                                 )
                                          ]

        -- FIXME css
        renderVideo url p = block_ "media video" [
          VD.h "iframe" (p_ $ [ ("src", url)
                              , ("width", "600px")
                              , ("height", "430px")
                              , ("style", "margin-left: 50px;")
                              , ("frameborder", "0")
                              , ("allowfullscreen", "")] <> p) []]
        renderYoutube url = renderVideo ("https://www.youtube.com/embed/" <> url <> "?rel=0") [("class", "youtube")]
        renderVimeo id = renderVideo ("https://player.vimeo.com/video/" <> id) [("class", "vimeo")]

        renderInstagram url = block_ "media video"
          [ VD.h "div" (p_ [("style", "background:#FFF; border:0; border-radius:3px; box-shadow:0 0 1px 0 rgba(0,0,0,0.5),0 1px 10px 0 rgba(0,0,0,0.15); margin: 1px; max-width:600px; padding:0; width: 600px; margin-left: 50px;")])
            [ VD.h "div" (p_ [("style", "padding:8px;")])
              [ VD.h "div" (p_ [("style", "background:#F8F8F8; line-height:0; padding:0; text-align:center; width:100%;")])
                [ VD.h "img" (p_ [ ("src", "https://instagram.com/p/" <> url <> "/media/?size=l")
                                 , ("style", "display:block; margin:0; position:relative; top:0; width:100%; height: auto;")]) []
                ]
              ]
            ]
          ]

        renderTweet tid =
          let z = read tid -- XXX
          in case HM.lookup z adhoc of
            Nothing -> block_ "media embedded-tweet" [ panel' [VD.text $ "Embedded tweet ", link (T.pack $ mkTweetUrl' z Nothing) (T.pack tid)] ]
            Just t  -> block_ "media embedded-tweet" [ tweet t ]

        entities e = goMedia e <> goUrls e
          where
            goMedia e = case BL.media e of
              Just xs -> flip fmap xs $ \m -> case BL.mType m of
                "photo" -> renderMediaImage m
                x       -> VD.text $ "Unknown media type: " <> x
              Nothing -> []

            goUrls e = catMaybes $ flip fmap (BL.urls e) $ \u -> asum [ matchYoutube u, matchInstagram u, matchVimeo u, matchTwitter u ]
            matchYoutube = matchFn renderYoutube youtubePattern
            matchInstagram = matchFn renderInstagram instagramPattern
            matchVimeo = matchFn renderVimeo vimeoPattern
            matchTwitter = matchFn renderTweet twitterPattern

            matchFn renderFn p u =
              fmap (renderFn . JSS.unpack . head . RegExp.subMatched)
                   (RegExp.exec (JSS.pack $ BL.eExpandedUrl u) p)

        toolbarStyle = A [ ("class", "tweet-toolbar") ]
        toolbarStyleA = A [ ("class", "tweet-toolbar-a") ]
        toolbarStyleB = A [ ("class", "tweet-toolbar-b") ]
        toolbarBtnStyle = A [ ("class", "tweet-toolbar-button") ]

        isRt t = case BL.status_retweeted t of
          Just True -> True
          otherwise -> False

        isLv t = case BL.status_favorited t of
          Just True -> True
          otherwise -> False

        statusRtCss t  = if isRt t then "display: block !important;"    else ""
        statusRtCss' t = if isRt t then "background-color: lightgreen;" else ""

        statusLkCss t  = if isLv t then "display: block !important;"    else ""
        statusLkCss' t = if isLv t then "background-color: lightgreen;" else ""

        toolbarRt t = VD.h "span"
                         (p $ toolbarStyle <> A [("style", ("top: 0px;" <> statusRtCss t))])
                         [ buttonIcon "" "retweet"
                                      (if isRt t then "Unretweet" else "Retweet")
                                      (toolbarBtnStyle <> A [("style", statusRtCss' t)])
                                      [ onClick_ $ tweetActionU (Retweet t (not $ isRt t)) ] ]
        toolbarRe t = VD.h "span"
                         (p $ toolbarStyle <> A [("style", "top: 30px;")])
                         [ buttonIcon "" "comment"       "Reply"   toolbarBtnStyle [ onClick_ $ tweetActionU (Reply t) ] ]

        toolbarLk t = VD.h "span"
                         (p $ toolbarStyle <> A [("style", ("top: 60px;" <> statusLkCss t))])
                         [ buttonIcon "" "heart"
                                      (if isLv t then "Unlove" else "Love")
                                      (toolbarBtnStyle <> A [("style", statusLkCss' t)])
                                      [ onClick_ $ tweetActionU (Love t (not $ isLv t)) ] ]

        toolbarGo t = VD.h "span"
                         (p $ toolbarStyle <> A [("style", "top: 90px")])
                         [ buttonIcon "" "external-link" "Open in Twitter" toolbarBtnStyle [ onClick_ $ tweetActionU (Go t) ] ]

        authorToolbar s u = VD.h "span"
                         (p s)
                         [ -- VD.h "span" (p_ []) [VD.text . T.unpack $ BL.name u]
                           buttonIcon "" "user-circle-o" "Info" toolbarBtnStyle [ onClick_ $ tweetActionU (UserInfo u) ]
                         , buttonIcon "" "list-ul"  "Feed" toolbarBtnStyle [ onClick_ $ tweetActionU (UserFeed u) ]
                         ]

        authorNameToolbar s u = VD.h "span"
                         (p $ s <> A [("style", "top: 44px; width: auto; background-color: #eee; padding: 5px 10px 5px 10px; height: auto; white-space: nowrap;"), ("class", "author-name-toolbar")])
                         [ VD.h "span" (p_ []) [VD.text . T.unpack $ BL.name u] ]

        author t = case (BL.user t, BL.user <$> BL.retweet t) of
          (a, Nothing) -> m "user-icon user-icon-x" toolbarStyleA a
          (a, Just b)  ->
            VD.h "span"
                  (p_ [("class", "user-icon")])
                  [ m "user-icon1 user-icon-x" toolbarStyleA a
                  , m "user-icon2 user-icon-y" toolbarStyleB b ]
          where
            m = \c s a -> VD.h "span"
                             (p_ [("class", c)])
                             [ authorToolbar s a
                             , authorNameToolbar s a
                             , VD.h "a"
                                   (p_ [("href", T.unpack "javascript:void(0)"), ("target", "_blank")])
                                   [flip VD.with [ onClick_ (requestUserInfoU (RequestUserInfo . T.unpack $ BL.screen_name a))] $
                                       VD.h "img"
                                         (p_ [ ("class", "user-icon-img")
                                             , ("src", BL.profile_image_url a)
                                             , ("title", T.unpack $ BL.name a)])
                                         []
                                   ]
                             ]


        body t = block_ "tweet-body" $ if isJust (BL.media . BL.entities $ t)
                    && isLink (DL.last $ BL.text t)
                    && isNothing (resolveLink t . (\(BL.Link s) -> s) . DL.last . BL.text $ t)
                 then fmap (telToHtml t) (DL.init $ BL.text t)
                 else fmap (telToHtml t) (BL.text t)

        telToHtml t (BL.AtUsername s) = VD.h "span" (p_ [("class", "username-tag")]) [link ("https://twitter.com/" <> s) ("@" <> s)]

        telToHtml t (BL.Link s) =
          case resolveLink t s of
            Nothing -> inlineLabel_ $ link' "inline-link" s (T.pack . displayUri . T.unpack $ s)
            Just x  -> inlineLabel_ $ link' "inline-link" (T.pack $ BL.eExpandedUrl x) (T.pack . displayUri $ BL.eExpandedUrl x)

          where
            displayUri u =
              let x = do
                        mu <- NU.parseURI u
                        ma <- NU.uriAuthority mu
                        pure $ NU.uriRegName ma
              in fromMaybe u x

        telToHtml t (BL.PlainText s)  = inlineLabel $ decodeHtmlEntities $ s
        telToHtml t (BL.Hashtag s)    = VD.h "span" (p_ [("class", "hash-tag")]) [link ("https://twitter.com/hashtag/" <> s <> "?src=hash") ("#" <> s)]
        telToHtml t BL.Retweet        = inlineLabel "Retweet"

        telToHtml t BL.LineBreak      = lineBreak

        telToHtml t (BL.Spaces s)     = inlineLabel s
        telToHtml t (BL.Unparsable s) = inlineLabel s

        resolveLink t s = listToMaybe $ filter ((s ==) . T.pack . BL.eUrl) (BL.urls . BL.entities $ t)

        decodeHtmlEntities = TL.toStrict . TLB.toLazyText . HE.htmlEncodedText

        isLink (BL.Link _) = True
        isLink _           = False
