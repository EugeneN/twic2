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
import Data.Maybe                    (Maybe(..), isJust)
import Data.Monoid
import qualified Data.Set            as Set

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD

import qualified BL.Types           as BL
import BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.WebSocket
import Lib.UI


data TestWSBLAction = AddNew BL.Tweet | ShowNew | ShowOld Int deriving (Show, Eq)
type Feed = ([BL.Tweet], [BL.Tweet], [BL.Tweet])

testWS :: TheApp t m l Counter
testWS = do
  (controllerE :: R.Event t TestWSBLAction, controllerU) <- RHA.newExternalEvent
  (modelE :: R.Event t (Either String WSData), modelU) <- RHA.newExternalEvent
  (tweetsE :: R.Event t BL.Tweet, tweetsU) <- RHA.newExternalEvent

  feedD  <- R.foldDyn feedOp ([],[],[]) controllerE
  modelD <- R.foldDyn (\x xs -> xs <> [x]) [] modelE

  (wsi, wsready) <- setupWebsocket socketUrl
  wsReady <- R.headE . R.ffilter isJust . R.updated $ wsready

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

    render :: Sink TestWSBLAction -> Feed -> VD.VNode l
    render controllerU (old, cur, new) = block [historyButton, tweetList cur, refreshButton new] where
      historyButton = VD.h "div"
        (VD.prop [("style", "text-align: center; margin-top: 15px;")])
        [button "..." ([("id", "load-history-tweets-id"), ("class", "history-button")]) [VD.On "click" (void . const (controllerU (ShowOld 1)))]]
      tweetList cur = container [list $ if DL.null cur then [noTweetsLabel "EOF"] else (fmap renderTweet cur)]
      refreshButton new = VD.h "div" (VD.prop [("class", "refresh")]) [button (show $ length new)
                          (unA $ A [("class", if not (null new) then "there-are-new-tweets" else "no-new-tweets")])
                          [VD.On "click" (void . const (controllerU ShowNew))]]

    renderTweet t = block [tweet t]
