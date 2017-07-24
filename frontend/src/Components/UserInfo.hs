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

userinfoComponent :: TheApp t m l Counter
userinfoComponent = do
  liftIO $ print "Hello UserInfo"
  return (pure (panel [ textLabel "Hello UserInfo" ]), pure (Counter 0))
