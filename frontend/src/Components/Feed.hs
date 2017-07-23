{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Components.Feed where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Monad                 (void, forM_, when)

import qualified Data.List           as DL
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe)
import Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified JavaScript.Web.WebSocket as WS

import qualified BL.Types           as BL
import BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.UI


allButLast n [] = []
allButLast n xs = DL.init xs

last_ n [] = []
last_ n xs = [DL.last xs]

data FeedAction = AddNew BL.Tweet | ShowNew | ShowOld Int deriving (Show, Eq)
type Feed = ([BL.Tweet], [BL.Tweet], [BL.Tweet])

feedComponent :: R.Event t ChildAction
              -> (WSInterface t, R.Event t (Maybe WS.WebSocket))
              -> TheApp t m l Counter
feedComponent parentControllerE (wsi, wsReady) = do
  (controllerE :: R.Event t FeedAction, controllerU) <- RHA.newExternalEvent
  (modelE :: R.Event t (Either String WSData), modelU) <- RHA.newExternalEvent
  (tweetsE :: R.Event t BL.Tweet, tweetsU) <- RHA.newExternalEvent

  feedD  <- R.foldDyn feedOp ([],[],[]) controllerE
  modelD <- R.foldDyn (\x xs -> xs <> [x]) [] modelE

  ws_rcve wsi ~> (print . mappend "Received from WS: " . show)
  ws_rcve wsi ~> modelU

  subscribeToEvent modelE $ \x -> case x of
    Right (WSData xs) -> forM_ xs $ \y -> when (isTweet y) $ tweetsU (unpackTweet y) >> pure ()
    otherwise -> pure ()

  subscribeToEvent tweetsE $ \x -> controllerU (AddNew x) >> pure ()

  let ownViewDyn = fmap (render controllerU) feedD

  return (ownViewDyn, pure (Counter 0))

  where
    feedOp op (old, cur, new) = case op of
      AddNew t  -> (old, cur, (unique $ new <> [t]))
      ShowNew   -> ((unique $ old <> cur), new, [])
      ShowOld n -> (allButLast n old, (unique $ last_ n old <> cur), new)

    unique = Set.toAscList . Set.fromList

    unpackTweet (BL.TweetMessage t) = t

    isTweet (BL.TweetMessage _) = True
    isTweet _                   = False

    render :: Sink FeedAction -> Feed -> VD.VNode l
    render controllerU (old, cur, new) =
      block [historyButton, tweetList cur, refreshButton new]

      where
        historyButton =
          VD.h "div"
            (VD.prop [("style", "text-align: center; margin-top: 15px;")])
            [button "..." ([("id", "load-history-tweets-id"), ("class", "history-button")])
                          [VD.On "click" (void . const (controllerU (ShowOld 1)))]
            ]

        tweetList cur = container [list $ if DL.null cur then [noTweetsLabel "EOF"] else (fmap (block . (: []) . tweet) cur)]

        refreshButton new =
          VD.h "div"
            (VD.prop [("class", "refresh")])
            [button (show $ length new)
                    (unA $ A [("class", if not (null new) then "there-are-new-tweets" else "no-new-tweets")])
                    [VD.On "click" (void . const (controllerU ShowNew))]
            ]

        tweet t = panel' $ [ author (BL.user t), body t ] <> entities (BL.entities t)

        entities e = case BL.media e of
          Just xs -> flip fmap xs $ \m -> VD.h "div"
                                            (p_ [("class", "media")])
                                            [link_ (T.pack $ BL.mMediaUrl m)
                                                   (VD.h "img"
                                                         (p_ [ ("class", "inline-img")
                                                             , ("src", BL.mMediaUrl m)
                                                             , ("title", "")] )
                                                         []
                                                   )
                                            ]

          otherwise -> []

        author a = VD.h "span"
                        (p_ [("class", "user-icon")])
                        [VD.h "span"
                              (p_ [("class", "user-icon")])
                              [VD.h "img"
                                    (p_ [ ("class", "user-icon-img")
                                        , ("src", BL.profile_image_url a)
                                        , ("title", T.unpack $ BL.name a)])
                                    []
                              ]
                        ]

        body t = if isJust (BL.media . BL.entities $ t)
                    && isLink (DL.last $ BL.text t)
                    && isNothing (resolveLink t . (\(BL.Link s) -> s) . DL.last . BL.text $ t)
                 then block_ "tweet-body" (fmap (telToHtml t) (DL.init $ BL.text t))
                 else block_ "tweet-body" (fmap (telToHtml t) (BL.text t))

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
