{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Types where

import Control.Monad.Fix             (MonadFix)

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified Data.VirtualDOM.DOM as DOM

import qualified BL.Types           as BL

data Counter       = Counter Int deriving (Show)

data UserInfoQuery = RequestUserInfo String -- XXX FIXME

data Notification  = 
  Error { title :: String, body :: String } 
  | Info { title :: String, body :: String } 
  | Warning { title :: String, body :: String }
  | Success { title :: String, body :: String } deriving Show

-- data Notification = forall a . Error String | Info a deriving Show

-- `l` is DOM.Node in currently; polymorphic to enable other implementations
type AppContainer t m l c = (RHA.MonadAppHost t m, l ~ DOM.Node, c ~ Counter) => l -> TheApp t m l c -> m ()
type AppHost              = (forall t m l c . (l ~ DOM.Node, c ~ Counter) => AppContainer t m l c)
                         -> (forall t m l c . c ~ Counter => TheApp t m l c)
                         -> IO ()
type TheApp t m l c       = (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), R.Dynamic t c)
type Sink a               = a -> IO Bool

type ViewDyn t l     = R.Dynamic t (VD.VNode l)

data ChildAction     = Reset deriving (Eq)

data WSData = WSData BL.FeedState | WSCommand String deriving (Show)

data WSInterface t = WSInterface
  { ws_send    :: WSData -> IO (Either String Bool)
  , ws_rcve    :: R.Event t (Either String WSData)
  }
