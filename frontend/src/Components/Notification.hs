{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}

module Components.Notification where

import qualified BL.Types            as BL
import Control.Applicative
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad.Fix             (MonadFix)
import Data.Functor                  (void)
import qualified Data.List           as DL
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe)
import Data.Monoid
import qualified Data.Text           as T
import qualified Data.VirtualDOM     as VD
import Lib.FRP
import Lib.UI
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import Types

data UpdCmd = Append Notification | RemoveByIdx Int

notificationComponent :: (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), Sink Notification)
notificationComponent = do
  (updEvent :: R.Event t UpdCmd, updU) <- RHA.newExternalEvent
  modelD <- R.foldDyn action [] updEvent
  
  -- only for debug
  subscribeToEvent (R.updated modelD) $ \n -> do
    print $ "Length => " <> show (length n)
    return ()
  
  let apiU = updU . Append
  let view = render updU <$> modelD
  
  return (view, apiU)
  
  where
    action c acc = case c of
      Append n -> acc <> [n]
      RemoveByIdx i -> (\(a,b) -> a  <> drop 1 b) . DL.splitAt i $ acc

    container s x = block_ ("notification-wrapper " <> s) [x]
    
    render _ [] = container "hide fadeOut" mempty
    render updU notifications = container "animated fadeIn" $ items updU notifications
    
    items updU notifications = block_ "notification-wrapper-inner" (item updU <$> zip [0..] notifications)
    
    item updU (index, n) = case n of
      Info {..} -> template "Info" updU index title body
      Error {..} -> template "Error" updU index title body
      Warning {..} -> template "Warning" updU index title body

    template t updU index title body = VD.h "div" (p_ [("style", "top:" <> show(65 * if index == 0 then 0 else index * 2) <> "px"), ("class", "notification-item-info notification-item-info-" <> show index)])
      [ block_ "notification-item-info-header" [VD.text $ if null title then t else t <> " :" <> title, closeButton updU index]
      , block_ "notification-item-info-body" [VD.text body]]
        
    closeButton updU index =
      button "X" 
        [("class", "close-button")] 
        [onClick (const (print ("Index for delete => " <> show index) >> updU (RemoveByIdx index) >> pure ()))]