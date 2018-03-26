{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards #-}

module Components.Login where

import qualified Data.Text           as T

import Control.Concurrent            (forkIO)
import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Fix             (MonadFix)

import Control.Monad                 (void)
import Data.Monoid

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD

import qualified Data.JSString       as JSS

import Types
import Lib.FRP
import Lib.UI
import Lib.FW
import Lib.Net (getAPI)

import qualified BL.Types           as BL
import BL.Instances

loginComponent :: (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), Sink Bool)
loginComponent = do
    (bE :: R.Event t Bool, bU) <- RHA.newExternalEvent

    bD <- R.holdDyn True bE

    subscribeToEvent bE $ \_ -> do
        void . forkIO $ do
            -- windowOpen $ JSS.pack $ "https://google.com"
            (x :: Either String BL.LoginInfo) <- liftIO . getAPI . JSS.pack $ "http://localhost:3000/login"

            case x of
                Right (BL.LoginInfo t) -> redirect $ JSS.pack $ T.unpack t
                Left e -> print $ "Error => " <> show e

            print $ "LoginInfo => " <> show x
            pure ()

    let v = fmap (render bU) bD
    
    return (v, bU)
    
    where
        render bU _ = button "Login"
            (p_ [("style", "")])
            [ onClick_ $ bU True ]