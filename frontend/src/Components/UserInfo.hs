{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Components.UserInfo where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Monad                 (void, forM_, when)
import Control.Monad.IO.Class        (liftIO)

import Control.Monad.Fix             (MonadFix)

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


-- data UserInfoQuery = RequestUserInfo String

xhrJsonGet :: String -> IO String
xhrJsonGet s = return $ "xhrJsonGet " <> s

userinfoComponent :: (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), Sink UserInfoQuery)
userinfoComponent = do
  liftIO $ print "Hello UserInfo"

  (queryE :: R.Event t UserInfoQuery, queryU) <- RHA.newExternalEvent
  (modelE :: R.Event t (Maybe String), modelU) <- RHA.newExternalEvent
  modelD <- R.holdDyn Nothing modelE

  subscribeToEvent queryE $ \(RequestUserInfo screenName) -> do
    x <- liftIO $ xhrJsonGet $ "http://localhost:3000/userinfo?sn=" <> screenName
    modelU $ Just x
    pure ()

  let v = fmap render modelD

  -- show panel
  -- listen for close panel events
  return (v, queryU)

  where
    render Nothing = panel [ textLabel "No UserInfo" ]
    render (Just x) = panel [ textLabel x ]
