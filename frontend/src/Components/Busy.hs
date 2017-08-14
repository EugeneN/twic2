{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}

module Components.Busy where

import qualified Data.VirtualDOM     as VD
import Control.Applicative
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad                 (when)
import Control.Monad.Fix             (MonadFix)
import Data.Monoid                   ((<>))

import Lib.FRP
import Lib.UI
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import Types

data BusyCmd = PushBusy | PopBusy
  
busyComponent :: (RHA.MonadAppHost t m, MonadFix m) => Sink Notification -> m (R.Dynamic t (VD.VNode l), Sink BusyCmd)
busyComponent ntU = do
  (busyE :: R.Event t BusyCmd, busyU) <- RHA.newExternalEvent
  modelD <- R.foldDyn action 0 busyE
  
  subscribeToEvent (R.updated modelD) $ \v -> do
    print $ "BusyFactor => " <> show v
    when(v < 0) $ ntU (Error "BusyError" "BusyError") >> pure ()
    pure ()  
  
  let view = render busyU <$> modelD
  
  return (view, busyU)
  
  where
    action c acc = case c of
      PushBusy -> acc + 1
      PopBusy -> acc - 1
      
    container s x = block_ ("busy-wrapper " <> s) [x]
    
    render busyU x
      | x < 0 = mempty
      | x == 0 = container "hide fadeOut" mempty
      | otherwise = container "animated fadeIn" $ block [VD.text "Loading..."]