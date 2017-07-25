{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}

module Components.UserInfo where

import Prelude
import Control.Applicative           ((<*>), (<$>))
import Control.Concurrent            (forkIO)
import Control.Monad                 (void, forM_, when)
import Control.Monad.IO.Class        (liftIO)

import Control.Monad.Fix             (MonadFix)

import qualified Data.List           as DL
import Data.Maybe                    (Maybe(..), isJust, isNothing, listToMaybe, maybe)
import Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T

import qualified Reflex              as R
import qualified Reflex.Host.App     as RHA

import qualified Data.VirtualDOM     as VD
import qualified JavaScript.Web.WebSocket as WS
import qualified Data.JSString      as JSS
import Web.Twitter.Types (User (..))


import qualified BL.Types           as BL
import BL.Instances

import UIConfig
import Types
import Lib.FRP
import Lib.FW
import Lib.UI
import Lib.Net (getAPI)


-- data UserInfoQuery = RequestUserInfo String

xhrJsonGet :: String -> IO String
xhrJsonGet s = return $ "xhrJsonGet " <> s

userinfoComponent :: (RHA.MonadAppHost t m, MonadFix m) => m (R.Dynamic t (VD.VNode l), Sink UserInfoQuery)
userinfoComponent = do
  (showE :: R.Event t Bool, showU) <- RHA.newExternalEvent
  showD <- R.holdDyn False showE

  (queryE :: R.Event t UserInfoQuery, queryU) <- RHA.newExternalEvent
  (modelE :: R.Event t (Either String BL.JsonUserInfo), modelU) <- RHA.newExternalEvent
  modelD <- R.holdDyn (Left "No data yet") modelE

  subscribeToEvent queryE $ \(RequestUserInfo screenName) -> do
    showU True
    void . forkIO $ do
      x <- liftIO . getAPI . JSS.pack $ "http://localhost:3000/userinfo?sn=" <> screenName
      modelU x
      pure ()

  let v = fmap (render showU modelU) $ (,) <$> modelD <*> showD

  return (v, queryU)

  where

    userInfoStyleStatic = DL.intercalate ";"
        [ "display: block"
        , "height: 460px"
        , "width: 100%"
        , "position: fixed"
        , "top: 50%"
        , "left: 0"
        , "overflow: hidden"
        , "box-shadow: rgb(169, 169, 169) 0px 10px 50px"
        , "color: white"
        , "transform: translateY(-300px)"
        , "-webkit-transform: translateY(-300px)"
        , "z-index: 1000" ]

    userInfoStyleDynamic User{..} = DL.intercalate ";"
        [ "background-color:" <> maybe "rgba(0,0,0,0.95)" (\c -> "#" <> T.unpack c) userProfileBackgroundColor
        , "background-image:" <> maybe "none" (\src -> "url(" <> T.unpack src <> ")") userProfileBannerURL
        , "background-size:" <> maybe "auto" (const "cover") userProfileBannerURL ]

    closeButton showU modelU =
      flip VD.with [VD.On "click" (void . const (showU False >> modelU (Left "No data yet") >> pure ()))] $
        VD.h "button"
             (p_ [("style", DL.intercalate ";" [ "position: absolute"
                                               , "top: 10px"
                                               , "right: 10px"
                                               , "color: white"
                                               , "background-color: #B0E57C"])])
             [VD.text "X"]

    followButton User{..} = maybe
      (VD.text "Unknown status for following")
      (\v -> if not v
        then flip VD.with [VD.On "click" (void . const (print "follow"))] $
          VD.h "button"
               (p_ [("style", DL.intercalate ";" [ "margin-left: 8px"
                                                 , "cursor: pointer"
                                                 , "border-radius: 10px"
                                                 , "border: 0 solid green"
                                                 , "background-color: #B0E57C"])])
               [VD.text "Follow"]
        else VD.text "Follow") userFollowRequestSent
    unFollowButton User{..} = maybe
      (VD.text "Unknown status for following")
      (\x -> if not x
        then flip VD.with [VD.On "click" (void . const (print "unfollow"))] $
          VD.h "button"
               (p_ [("style", DL.intercalate ";" [ "margin-left: 8px"
                                                 , "cursor: pointer"
                                                 , "border-radius: 10px"
                                                 , "border: 0 solid red"
                                                 , "background-color: #B0E57C"])])
               [VD.text "Unfollow"]
        else VD.text "Unfollow") userFollowRequestSent

    popupStyle =  A [("style", DL.intercalate ";" [ "display: block" , "width: 600px" , "text-align: left"
                                                  , "margin: auto" , "overflow: hidden" , "padding: 20px"
                                                  , "background-color: rgba(0,0,0,0.3)" , "height: 420px" ] )]

    renderUser user@User{..} =
      VD.h "ul"
           (p popupStyle)
           [ VD.h "li"
                  (p_ [("style", DL.intercalate ";" [ "margin: 0", "padding: 5px", "padding-top: 5px", "margin-top: -5px"])])
                  [VD.h "span"
                        (p_ [("style", "font-size: 200%")])
                        [ VD.text . T.unpack $ userName
                        ,  if userVerified
                            then VD.h "span" (p_ [("style", "color: blue")]) [VD.text " •"]
                            else VD.text ""
                        , if userProtected
                            then VD.h "span" (p_ [("style", "color: red")]) [VD.text " •"]
                            else VD.text ""]]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [VD.h "a"
                        (p_ [ ("style", "color: lightgrey")
                            , ("href", "https://twitter.com/" <> T.unpack userScreenName)
                            , ("target", "_blank")])
                        [VD.text $ "@" <> T.unpack userScreenName]
                  ]
           , VD.h "li"
                  (p_ [("style", "margin: 0; padding: 5px")])
                  [ maybe (VD.text "")
                          (\url -> VD.h "img" (p_ [ ("style", "width: 100px")
                                                  , ("src", T.unpack url)]) [])
                          userProfileImageURL]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [ VD.text $ maybe "" T.unpack userDescription]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [ maybe (VD.text "") (\url -> VD.h "a"
                                                     (p_ [ ("style", "color: white; text-decoration: underline")
                                                         , ("href", T.unpack url)])
                                                     [VD.text $ T.unpack url]) userURL]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [VD.text $ maybe "" T.unpack userLocation]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [maybe (VD.text "") (\tz -> VD.text $ T.unpack tz <> " timezone") userTimeZone]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [VD.text $ "Register on " <> show userCreatedAt]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [ VD.text $ show userFollowersCount <> " followers, "
                           <> show userFriendsCount <> " friends, "
                           <> show userStatusesCount <> " tweets"]
           , VD.h "li"
                  (p_ [("style", "margin:0; padding: 5px")])
                  [ maybe (VD.text "Following status unknown")
                          (\fol -> if fol
                            then VD.h "span" (p_ []) [VD.text "Already following", unFollowButton user]
                            else VD.h "span" (p_ []) [VD.text "Not following", followButton user]) userFollowing]
           ]

    cont x = VD.h "div" (p_ [("id", "userinfo-container-id")]) [x]

    render _ _ (_, False) = cont mempty
    render showU modelU (Left e, True) = cont $
      VD.h "div"
           (p $ A [("class", "user-info"), ("style", userInfoStyleStatic)])
           [closeButton showU modelU, VD.h "div" (p popupStyle) [errorLabel e]]

    render showU modelU (Right BL.JsonUserInfo{..}, True) = cont $
      VD.h "div"
           (p $ A [("class", "user-info"), ("style", userInfoStyleStatic <> userInfoStyleDynamic uiData)])
           [closeButton showU modelU, renderUser uiData]
