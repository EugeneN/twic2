{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Components.Notification where

import Control.Applicative
import Lib.UI
import Types
import Data.Monoid
import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA
import qualified Data.VirtualDOM     as VD
import Control.Monad.Fix             (MonadFix)
import Control.Concurrent            (forkIO, threadDelay)
import Lib.FRP
import Data.Functor                  (void)
import qualified BL.Types            as BL
import qualified Data.List           as DL
import qualified Data.Text           as T
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe)

notificationComponent :: (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), Sink (Notification String BL.Tweet))
notificationComponent = do
  (showE :: R.Event t Bool, showU) <- RHA.newExternalEvent
  showD <- R.holdDyn False showE
  
  (ntE :: R.Event t (Notification String BL.Tweet), ntU) <- RHA.newExternalEvent
  ntD <- R.holdDyn undefined ntE -- strange but I don't know what here should be
  
  subscribeToEvent ntE $ \e -> forkIO $ do
    showU True
    threadDelay 3000000 -- timeout 3s
    showU False
    pure ()
    
  let v = liftA2 (render showU ntU) ntD showD
    
  return (v, ntU)
  
  where
    -- TODO: move to file with styles
    contStyle = DL.intercalate ";" 
      [ "width: 360px"
      , "right: 20px"
      , "height: 80px"
      , "z-index: 10000"
      , "position: fixed"
      , "display: flex"
      , "box-shadow: 0 2px 5px 0 rgba(0, 0, 0, 0.16), 0 2px 10px 0 rgba(0, 0, 0, 0.12)"
      , "padding: 20px"
      , "margin: 0.5rem 0 1rem 0"
      , "border-radius: 2px"
      , "background-color: #fff"
      , "-webkit-transition: all ease .4s"
      , "-moz-transition: all ease .4s"
      , "-o-transition: all ease .4s"
      , "-ms-transition: all ease .4s"
      , "transition: all ease .4s"]
    cont s = VD.h "div" (p_ [("class", "notification-wrapper"), ("style", s)])
    
    author t = case (BL.user t, BL.user <$> BL.retweet t) of
      (a, Nothing) -> m "notification-icon" a
      (a, Just b)  ->
        VD.h "div"
              (p_ [("class", "notification-icon")])
              [ m "" a
              , m "notification-icon2" b ]
      where
        m c a = VD.h "div"
                         (p_ [("class", c)])
                         [VD.h "a"
                               (p_ [("href", T.unpack $ "https://twitter.com/" <> BL.screen_name a), ("target", "_blank")])
                               [VD.h "img"
                                     (p_ [ ("class", "user-icon-img")
                                         , ("src", BL.profile_image_url a)
                                         , ("title", T.unpack $ BL.name a)])
                                     []
                               ]
                         ]
    
    body t = if isJust (BL.media . BL.entities $ t)
                && isLink (DL.last $ BL.text t)
                && isNothing (resolveLink t . (\(BL.Link s) -> s) . DL.last . BL.text $ t)
             then block_ "notification-body" (fmap (telToHtml t) (DL.init $ BL.text t))
             else block_ "notification-body" (fmap (telToHtml t) (BL.text t))

    telToHtml t (BL.AtUsername s) = VD.h "span" (p_ [("class", "username-tag")]) [link ("https://twitter.com/" <> s) ("@" <> s)]

    telToHtml t (BL.Link s) = case resolveLink t s of
        Nothing -> inlineLabel_ $ link' "inline-link" s s
        Just x  -> inlineLabel_ $ link' "inline-link" (T.pack $ BL.eExpandedUrl x) (T.pack $ BL.eDisplayUrl x)

    telToHtml t (BL.PlainText s)  = inlineLabel s
    telToHtml t (BL.Hashtag s)    = VD.h "span" (p_ [("class", "hash-tag")]) [link ("https://twitter.com/hashtag/" <> s <> "?src=hash") ("#" <> s)]
    telToHtml t BL.Retweet        = inlineLabel "Retweet"
    telToHtml t (BL.Spaces s)     = inlineLabel s
    telToHtml t (BL.Unparsable s) = inlineLabel s

    resolveLink t s = listToMaybe $ filter ((s ==) . T.pack . BL.eUrl) (BL.urls . BL.entities $ t)

    isLink (BL.Link _) = True
    isLink _           = False
    
    render _ _ _ False = cont "dispay: none" mempty
    render showU ntU (Info t) True = cont contStyle [author t, body t]
    render showU ntU (Error s) True = undefined