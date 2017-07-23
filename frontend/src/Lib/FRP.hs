{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Lib.FRP where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Monad                 (void)
import Control.Monad.IO.Class        (liftIO)

import qualified Reflex              as R
import qualified Reflex.Class        as RC
import qualified Reflex.Host.App     as RHA


subscribeToEvent ev f = RHA.performEvent_ $ fmap (liftIO . void . f) ev
subscribeToEvent' ev f = RHA.performEvent_ $ fmap f ev

whenReady f = do
  ready <- RHA.getPostBuild
  subscribeToEvent ready f

(~>) :: RHA.MonadAppHost t m => R.Event t a -> (a -> IO b) -> m ()
(~>) ev sink = RHA.performEvent_ $ (liftIO . void . sink) <$> ev
