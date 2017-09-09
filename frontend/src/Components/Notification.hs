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

data UpdCmd = Append Notification | RemoveByIdx Int | RemoveAll

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
      RemoveAll -> []

    container x = block_ "notification-wrapper" [x]

    render _ [] = container mempty
    render updU notifications = container $ items updU notifications

    items updU notifications = block_ "notification-wrapper-inner" (item c updU <$> zip [0..] notifications)
      where c = length notifications

    item c updU (index, n) = case n of
      Info {..}    -> template c "Info"    updU "notification-info"    index title body
      Error {..}   -> template c "Error"   updU "notification-error"   index title body
      Warning {..} -> template c "Warning" updU "notification-warning" index title body
      Success {..} -> template c "Success" updU "notification-success" index title body

    attrs cls index = [ ("class", unwords [ "notification-item"
                                          , "animated"
                                          , "fadeIn"
                                          , "notification-item-" <> show index
                                          , cls])]

    template c t updU cls index title body = VD.h "div" (p_ $ attrs cls index)
      [ block_ "notification-item-header" $ [VD.h "span" (p_ []) [VD.text $ if null title then t else t <> " : " <> title]] <>
                                                 [VD.h "span" (p_ []) [closeAllButton updU] | c > 1, index == 0] <>
                                                 [VD.h "span" (p_ []) [closeButton updU index]]
      , block_ "notification-item-body" [VD.text body]]

    closeAllButton updU =
      button "сlose all"
        (p_ [("class", "close-all-button")])
        [ onClick_ $ updU RemoveAll ]

    closeButton updU index =
      button "×"
        (p_ [("class", "close-button")])
        [ onClick_ $ print ("Index for delete => " <> show index) >> updU (RemoveByIdx index)]
