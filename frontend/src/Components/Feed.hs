{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Components.Feed where

import Prelude
import Control.Applicative           ((<*>), (<$>), (<|>))
import Control.Concurrent            (forkIO)
import Control.Monad                 (void, forM_, when, join)
import Control.Monad.IO.Class        (liftIO)

import qualified Data.HashMap.Strict as HM
import qualified Data.List           as DL
import Data.Foldable                 (asum)
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe, catMaybes, fromMaybe)
import Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy      as TL

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified JavaScript.Web.WebSocket as WS
import qualified Data.JSString      as JSS
import qualified Data.JSString.RegExp as RegExp

import qualified HTMLEntities.Decoder as HE

import qualified BL.Types           as BL
import BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.UI
import Lib.Net (getAPI)
import Components.Busy


allButLast n [] = []
allButLast n xs = DL.init xs

last_ n [] = []
last_ n xs = [DL.last xs]

data FeedAction = AddNew BL.Tweet | ShowNew | ShowOld Int | Search | WriteNew deriving (Show, Eq)
data TweetAction = Retweet BL.Tweet | Reply BL.Tweet | Love BL.Tweet | Go BL.Tweet
                 | UserInfo BL.Author | UserFeed BL.Author deriving (Show, Eq)
type Feed = ([BL.Tweet], [BL.Tweet], [BL.Tweet])

data ThreadElem = T BL.Tweet | Separator

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

  feedD  <- R.foldDyn feedOp ([],[],[]) $ R.ffilter isFeedOp controllerE
  modelD <- R.foldDyn (\x xs -> xs <> [x]) [] modelE

  (adhocCmdE :: R.Event t BL.TweetId, adhocCmdU) <- RHA.newExternalEvent
  (adhocE :: R.Event t BL.FeedMessage, adhocU) <- RHA.newExternalEvent

  let adhocE' = fmap filterSelfLinks $ R.fmapMaybe unpackTweets adhocE
  adhocD <- R.foldDyn (\x xs -> HM.insert (BL.id x) x xs) HM.empty adhocE'

  let allTweetsD = (,) <$> feedD <*> adhocD

  subscribeToEvent adhocE' (handleAdhocEvents adhocCmdU)
  subscribeToEvent' adhocCmdE $ loadAdhocTweet allTweetsD adhocU
  ws_rcve wsi ~> (print . mappend "Received from WS: " . show)
  ws_rcve wsi ~> modelU
  subscribeToEvent (R.ffilter (not . isFeedOp) controllerE) print
  subscribeToEvent modelE (handleModelEvents tweetsU)
  subscribeToEvent tweetsE (handleNewTweets controllerU adhocCmdU)
  subscribeToEvent (R.updated feedD) setTitle'
  subscribeToEvent tweetActionE (handleTweetActions requestUserInfoU)

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

    handleTweetActions requestUserInfoU c = forkIO . void $ case c of
      Retweet t -> do
        x :: Either String (BL.TheResponse) <- withBusy busyU .
                              getAPI . JSS.pack $ "/retweet/?id=" <> show (BL.id t)
        case x of
          Left e  -> ntU $ Error "Retweet failed" e
          Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
          Right (BL.Ok (BL.JsonResponse _ fs))  -> ntU $ Success "Retweeted!" (mkTweetLink_ fs)

      Reply t -> do
        print "TODO reply component" >> pure False

      Go t -> do
        windowOpen $ JSS.pack $ mkTweetUrl t
        pure True

      Love t -> do
        x :: Either String (BL.TheResponse) <- withBusy busyU .
                                getAPI . JSS.pack $ "/star/?id=" <> show (BL.id t)
        case x of
          Left e -> ntU $ Error ":-(" e
          Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
          Right (BL.Ok (BL.JsonResponse _ fs))  -> ntU $ Success "Loved the tweet!" (mkTweetLink_ fs)

      UserInfo u -> do
        requestUserInfoU (RequestUserInfo . T.unpack $ BL.screen_name u)

      UserFeed u -> do
        x :: Either String (BL.TheResponse) <- withBusy busyU .
                                getAPI . JSS.pack $ "/userfeed/?sn=" <> show (BL.screen_name u)
        case x of
          Left e -> ntU $ Error ":-(" e
          Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
          Right (BL.Ok (BL.JsonResponse _ fs))  -> ntU $ Success "Loaded user feed!" "..."

    setTitle' (_,_,new) =
      setTitle $ case length new of
                    0 -> "No new tweets"
                    1 -> "1 new tweet"
                    x -> show x <> " new tweets"

    mkTweetLink_ fs =
      let t' = join $ fmap unpackTweets $ listToMaybe fs
      in maybe ":-)" mkTweetUrl t'

    feedOp op (old, cur, new) = case op of
      AddNew t  -> (old, cur, (unique $ new <> [t]))
      ShowNew   -> ((unique $ old <> cur), new, [])
      ShowOld n -> (allButLast n old, (unique $ last_ n old <> cur), new)
      _         -> (old, cur, new)

    isFeedOp x = case x of
      AddNew _  -> True
      ShowNew   -> True
      ShowOld _ -> True
      _         -> False

    unique = Set.toAscList . Set.fromList

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
        ((old, cur, new), adhoc) <- R.sample $ R.current allTweetsD
        let x = listToMaybe $ filter ((tid ==) . BL.id) (old <> cur <> new)
        let y = HM.member tid adhoc

        case (x, y) of
          (Nothing, False) -> liftIO $ void . forkIO $ do
            x :: Either String BL.TheResponse <- withBusy busyU .
                                    getAPI . JSS.pack $ "/adhoc/?id=" <> show tid
            case x of
              Left e  -> (ntU $ Error "Adhoc request for parent thread failed" e) >> pure ()
              Right (BL.Fail (BL.JsonApiError t m)) -> (ntU $ Error (T.unpack t) (T.unpack m)) >> pure ()
              Right (BL.Ok (BL.JsonResponse _ ts)) -> forM_ ts adhocU

          otherwise -> pure ()

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
    render controllerU requestUserInfoU tweetActionU ((old, cur, new), adhoc) =
      block [historyButton, tweetList (old, cur, new), refreshButton new]

      where
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

        groupByParent :: ([BL.Tweet], [[BL.Tweet]]) -> [[BL.Tweet]]
        groupByParent ([], acc) = acc
        groupByParent ((x:rest), acc) =
          let ps = findParents rest x
              ps' = ps <> [x]
              rest' = rest DL.\\ ps'
          in groupByParent (rest', acc <> [ps'])

        -- XXX inefficient algo; proof of concept only; TODO refactor later
        findParents :: [BL.Tweet] -> BL.Tweet -> [BL.Tweet]
        findParents us t =
          let p = filter (\t' -> Just (BL.id t') == BL.statusInReplyToStatusId t) us
              ps = join $ findParents us <$> p
          in ps <> p

        findChildren :: [BL.Tweet] -> BL.Tweet -> [BL.Tweet]
        findChildren us t =
          let p = filter (\t' -> BL.statusInReplyToStatusId t' == Just (BL.id t)) us
              ps = join $ findChildren us <$> p
          in p <> ps

        us = unique $ old <> cur <> new <> HM.elems adhoc
        grouped' = DL.reverse . groupByParent $ (DL.reverse us, [])
        grouped = filter (\xs -> DL.last xs `DL.elem` cur) grouped'

        tweetList (old, cur, new) = container
          [list $ if DL.null cur then [noTweetsLabel "EOF"]
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

        tweet t = panelRel $ [ toolbar t, author t, body t ]
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

        renderTweet tid = case HM.lookup (read tid) adhoc of
          Nothing -> block_ "media embedded-tweet" [ panel [VD.text $ "Embedded tweet ", link (T.pack $ mkTweetUrl' tid Nothing) (T.pack $ show tid)] ]
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

        toolbar t = VD.h "span"
                         (p toolbarStyle)
                         [ buttonIcon "" "retweet"       "Retweet" toolbarBtnStyle [ onClick_ $ tweetActionU (Retweet t) ]
                         , buttonIcon "" "comment"       "Reply"   toolbarBtnStyle [ onClick_ $ tweetActionU (Reply t) ]
                         , buttonIcon "" "heart"         "Like"    toolbarBtnStyle [ onClick_ $ tweetActionU (Love t) ]
                         , buttonIcon "" "external-link" "Open in Twitter" toolbarBtnStyle [ onClick_ $ tweetActionU (Go t) ]
                         ]

        authorToolbar s u = VD.h "span"
                         (p s)
                         [ buttonIcon "" "user-circle-o" "Info" toolbarBtnStyle [ onClick_ $ tweetActionU (UserInfo u) ]
                         , buttonIcon "" "list-ul"  "Feed" toolbarBtnStyle [ onClick_ $ tweetActionU (UserFeed u) ]
                         ]

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

        telToHtml t (BL.Link s) = case resolveLink t s of
            Nothing -> inlineLabel_ $ link' "inline-link" s s
            Just x  -> inlineLabel_ $ link' "inline-link" (T.pack $ BL.eExpandedUrl x) (T.pack $ BL.eDisplayUrl x)

        telToHtml t (BL.PlainText s)  = inlineLabel $ decodeHtmlEntities $ s
        telToHtml t (BL.Hashtag s)    = VD.h "span" (p_ [("class", "hash-tag")]) [link ("https://twitter.com/hashtag/" <> s <> "?src=hash") ("#" <> s)]
        telToHtml t BL.Retweet        = inlineLabel "Retweet"
        telToHtml t (BL.Spaces s)     = inlineLabel s
        telToHtml t (BL.Unparsable s) = inlineLabel s

        resolveLink t s = listToMaybe $ filter ((s ==) . T.pack . BL.eUrl) (BL.urls . BL.entities $ t)

        decodeHtmlEntities = TL.toStrict . TLB.toLazyText . HE.htmlEncodedText

        isLink (BL.Link _) = True
        isLink _           = False
