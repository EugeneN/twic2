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
import Data.Maybe                    (Maybe(..))
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

    allButLast n [] = []
    allButLast n xs = DL.init xs

    last_ n [] = []
    last_ n xs = [DL.last xs]

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

        tweet t = panel' [ author (BL.user t), body (BL.text t) ]

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

        body t = block_ "tweet-body" (fmap telToHtml t)

        telToHtml (BL.AtUsername s) = inlineLabel $ "@" <> s
        telToHtml (BL.Link s)       = inlineLabel_ $ link' "inline-link" s s
        telToHtml (BL.PlainText s)  = inlineLabel s
        telToHtml (BL.Hashtag s)    = VD.h "span" (p_ [("class", "hash-tag")]) [link ("https://twitter.com/hashtag/" <> s <> "?src=hash") ("#" <> s)] 
        telToHtml BL.Retweet        = inlineLabel "Retweet"
        telToHtml (BL.Spaces s)     = inlineLabel s
        telToHtml (BL.Unparsable s) = inlineLabel s
