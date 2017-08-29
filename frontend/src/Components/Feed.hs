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
import Control.Monad                 (void, forM_, when)

import qualified Data.HashMap.Strict as HM
import qualified Data.List           as DL
import Data.Foldable                 (asum)
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe, catMaybes)
import Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified JavaScript.Web.WebSocket as WS
import qualified Data.JSString      as JSS
import qualified Data.JSString.RegExp as RegExp

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
data TweetAction = Retweet BL.Tweet | Reply BL.Tweet | Love BL.Tweet deriving (Show, Eq)
type Feed = ([BL.Tweet], [BL.Tweet], [BL.Tweet])

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
  adhocD <- R.foldDyn (\x xs -> HM.insert (BL.id_ x) x xs) HM.empty $ fmap filterSelfLinks $ R.fmapMaybe unpackTweets adhocE

  subscribeToEvent adhocCmdE $ \id_ -> {-void . forkIO $ -} do
    x :: Either String BL.TheResponse <- withBusy busyU .
                            getAPI . JSS.pack $ "/adhoc/?id=" <> show id_
    case x of
      Left e  -> (ntU $ Error "Adhoc request failed" e) >> pure ()
      Right (BL.Fail (BL.JsonApiError t m)) -> (ntU $ Error (T.unpack t) (T.unpack m)) >> pure ()
      Right (BL.Ok (BL.JsonResponse _ ts)) -> forM_ ts adhocU

  ws_rcve wsi ~> (print . mappend "Received from WS: " . show)
  ws_rcve wsi ~> modelU

  subscribeToEvent (R.ffilter (not . isFeedOp) controllerE) print

  subscribeToEvent modelE $ \x -> case x of
    Right (WSData xs) -> forM_ xs $ \y -> when (isTweet y) $ tweetsU (unpackTweet y) >> pure ()
    _ -> pure ()

  subscribeToEvent tweetsE $ \x' -> do
    let x = filterSelfLinks x'
    controllerU (AddNew x)
    preloadEntities adhocCmdU x
    pure ()

  subscribeToEvent (R.updated feedD) $ \(_,_,new) ->
    setTitle $ case length new of
                  0 -> "No new tweets"
                  1 -> "1 new tweet"
                  x -> show x <> " new tweets"

  subscribeToEvent tweetActionE $ \c -> forkIO $ case c of
    Retweet t -> do
      x :: Either String (BL.TheResponse) <- withBusy busyU .
                            getAPI . JSS.pack $ "/retweet/?id=" <> show (BL.id_ t)
      case x of
        Left e  -> ntU $ Error "Retweet failed" e
        Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
        Right (BL.Ok _) -> ntU $ Info "Retweeted!" ":-)"

      print $ "Retweet result (TODO reply component): " <> show x -- TODO notification component

    Reply t -> do
      print "TODO reply component"
    Love t -> do
      x :: Either String (BL.TheResponse) <- withBusy busyU .
                              getAPI . JSS.pack $ "/star/?id=" <> show (BL.id_ t)
      case x of
        Left e -> ntU $ Error ":-(" e
        Right (BL.Fail (BL.JsonApiError t m)) -> ntU $ Error (T.unpack t) (T.unpack m)
        Right (BL.Ok _) -> ntU $ Info "Loved the tweet!" ":-)"

      print $ "Love result (TODO reply component): " <> show x

  let ownViewDyn = fmap (render controllerU requestUserInfoU tweetActionU) ((,) <$> feedD <*> adhocD)

  return (ownViewDyn, pure (Counter 0))

  where
    feedOp op (old, cur, new) = case op of
      AddNew t  -> (old, cur, (unique $ new <> [t]))
      ShowNew   -> ((unique $ old <> cur), new, [])
      ShowOld n -> (allButLast n old, (unique $ last_ n old <> cur), new)

    isFeedOp x = case x of
      AddNew _  -> True
      ShowNew   -> True
      ShowOld _ -> True
      _         -> False

    unique = Set.toAscList . Set.fromList

    filterSelfLinks t =
      let es = BL.entities t
          ls = BL.urls es
          newUrls = filter (go (BL.id_ t)) ls
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

    preloadEntities adhocCmdU t = void . forkIO $
      forM_ (BL.urls . BL.entities $ t) $ matchEntity (go (BL.id_ t) adhocCmdU) twitterPattern

      where
        go pid adhocCmdU x = do
          let y = read x
          when (y /= pid) $ adhocCmdU y >> pure ()
          -- TODO skip loading if already cached

    render :: Sink FeedAction -> Sink UserInfoQuery -> Sink TweetAction -> (Feed, HM.HashMap BL.TweetId BL.Tweet) -> VD.VNode l
    render controllerU requestUserInfoU tweetActionU ((old, cur, new), adhoc) =
      block [historyButton, tweetList cur, refreshButton new]

      where
        historyButton =
          VD.h "div"
            (VD.prop [("style", "text-align: center; margin-top: 15px;")])
            [ button "wr" (p_ [("id", "write-new-tweet-id"), ("class", "history-button")])
                           [ onClick_ $ controllerU WriteNew ]
            , button "..." (p_ [("id", "load-history-tweets-id"), ("class", "history-button")])
                          [ onClick_ $ controllerU (ShowOld 1) ]
            , button "se" (p_ [("id", "search-tweets-id"), ("class", "history-button")])
                          [ onClick_ $ controllerU Search ]
            ]

        tweetList cur = container [list $ if DL.null cur then [noTweetsLabel "EOF"] else fmap (block . (: []) . tweet) cur]

        refreshButton new =
          VD.h "div"
            (VD.prop [("class", "refresh")])
            [button (show $ length new)
                    (p_ [("class", if null new then "no-new-tweets" else "there-are-new-tweets")])
                    [ onClick_ (controllerU ShowNew >> scrollToTop) ]
            ]

        tweet t = panel' $ [ toolbar t, author t, body t ] <> entities (BL.entities t)

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
          Nothing -> block_ "media embedded-tweet" [ VD.text $ "Here will be embedded tweet " <> tid ]
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
        toolbarBtnStyle = A [ ("class", "tweet-toolbar-button") ]

        toolbar t = VD.h "span"
                         (p toolbarStyle)
                         [ button "RT" (p toolbarBtnStyle) [ onClick_ $ tweetActionU (Retweet t) ]
                         , button "RE" (p toolbarBtnStyle) [ onClick_ $ tweetActionU (Reply t) ]
                         , button "LV" (p toolbarBtnStyle) [ onClick_ $ tweetActionU (Love t) ]
                         , VD.h "a" (p $ toolbarBtnStyle <> A [ ("target", "_blank")
                                                              , ("href", "https://twitter.com/xxx/status/" <> show (BL.id_ t))])
                                                              [VD.text "GO"]
                         ]

        author t = case (BL.user t, BL.user <$> BL.retweet t) of
          (a, Nothing) -> m "user-icon" a
          (a, Just b)  ->
            VD.h "span"
                  (p_ [("class", "user-icon")])
                  [ m "user-icon1" a
                  , m "user-icon2" b ]
          where
            m = \c a -> VD.h "span"
                             (p_ [("class", c)])
                             [VD.h "a"
                                   (p_ [("href", T.unpack "javascript:void(0)"), ("target", "_blank")])
                                  --  (p_ [("href", T.unpack $ "https://twitter.com/" <> BL.screen_name a), ("target", "_blank")])
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

        telToHtml t (BL.PlainText s)  = inlineLabel s
        telToHtml t (BL.Hashtag s)    = VD.h "span" (p_ [("class", "hash-tag")]) [link ("https://twitter.com/hashtag/" <> s <> "?src=hash") ("#" <> s)]
        telToHtml t BL.Retweet        = inlineLabel "Retweet"
        telToHtml t (BL.Spaces s)     = inlineLabel s
        telToHtml t (BL.Unparsable s) = inlineLabel s

        resolveLink t s = listToMaybe $ filter ((s ==) . T.pack . BL.eUrl) (BL.urls . BL.entities $ t)

        isLink (BL.Link _) = True
        isLink _           = False
