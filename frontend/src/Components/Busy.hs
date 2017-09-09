{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}

module Components.Busy (withBusy, busyComponent, BusyCmd(..)) where

import qualified Data.VirtualDOM     as VD
import Control.Applicative
import Control.Monad                 (when, void)
import Control.Monad.Fix             (MonadFix)
import Data.Monoid                   ((<>))

import Lib.FRP
import Lib.UI
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import Types

data BusyCmd = PushBusy | PopBusy

withBusy u f = do
  u PushBusy
  r <- f
  u PopBusy
  return r

busyComponent :: (RHA.MonadAppHost t m, MonadFix m) => Sink Notification -> m (R.Dynamic t (VD.VNode l), Sink BusyCmd)
busyComponent ntU = do
  (busyE :: R.Event t BusyCmd, busyU) <- RHA.newExternalEvent
  modelD <- R.foldDyn action 0 busyE

  subscribeToEvent (R.updated modelD) $ \v ->
    when(v < 0) . void $ ntU (Error "BusyError" "BusyError")

  let view = render busyU <$> modelD

  return (view, busyU)

  where
    action c acc = case c of
      PushBusy -> acc + 1
      PopBusy -> max 0 (acc - 1)

    container s x = block_ ("busy-wrapper " <> s) [x]

    render busyU x
       | x > 0 = container "animated fadeIn" $ block [ VD.h "i" (p_ [("class", "fa fa-cog fa-spin")]) []
                                                     , VD.text $ " Loading... (" <> show x <> ")"]
       | otherwise = container "hide fadeOut" mempty
