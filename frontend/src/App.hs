{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module App (theApp) where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Monad                 (void, join)

import Data.Map.Strict               (Map)
import qualified Data.Map.Strict     as Map
import Data.Maybe                    (Maybe(..), isJust)
import Data.Monoid

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD

import qualified BL.Types           as BL
import  BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.UI
import Lib.WebSocket
import Components.Feed                (feedComponent)
import Components.UserInfo            (userinfoComponent)
import Components.Notification        (notificationComponent)


data AppBLAction = Something deriving (Show, Eq)


theApp :: TheApp t m l Counter
theApp = do
  (controllerE :: R.Event t AppBLAction, controllerU) <- RHA.newExternalEvent
  (childControllerE :: R.Event t ChildAction, childControllerU) <- RHA.newExternalEvent

  (wsi, wsready) <- setupWebsocket socketUrl
  wsReady <- R.headE . R.ffilter isJust . R.updated $ wsready

  (notificationComponentViewD, ntU) <- notificationComponent
  (userinfoComponentViewD, requestUserInfoU) <- userinfoComponent
  (feedComponentViewD, _) <- feedComponent childControllerE (wsi, wsReady) requestUserInfoU ntU

  let resultViewDyn = layout <$> notificationComponentViewD <*> feedComponentViewD <*> userinfoComponentViewD

  return (resultViewDyn, pure (Counter 0))

  where
    layout notification feed userinfo =
      notification <> columns [(feed, 100)] <> userinfo