{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Json where

import           BL.Core
import           BL.Types
import           Blaze.ByteString.Builder            (Builder)
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Data.Aeson
import qualified Data.ByteString.Lazy                as B
import qualified Data.Text                           as T
import           Debug.Trace                         (trace)
import           Network.HTTP.Conduit                (HttpException)
import           Web.Twitter.Types                   (User)


justFeedMessagesToJson :: Either (ApiError String) FeedState -> Builder
justFeedMessagesToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justFeedMessagesToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack x}

justFeedMessagesToJson (Right []) =
    fromLazyByteString $ encode JsonResponse {okFeedMessages = [], okTitle = "No new tweets"}

justFeedMessagesToJson (Right ts) =
    fromLazyByteString $ encode JsonResponse {okFeedMessages = ts, okTitle = T.pack $ show (length ts) ++ " new messages"}

--------------------------------------------------------------------------------

justUserToJson :: Either (ApiError String) User -> Builder
justUserToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justUserToJson (Right user) =
    fromLazyByteString $ encode JsonUserInfo {uiData = user, uiTitle = "userinfo"}

--------------------------------------------------------------------------------

justUserInfoToJson :: Either (ApiError String) User -> Builder
justUserInfoToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justUserInfoToJson (Right ui) =
    fromLazyByteString $ encode JsonUserInfo {uiData = ui, uiTitle = "userinfo"}

--------------------------------------------------------------------------------

justUnreadCountToJson :: Int -> Builder
justUnreadCountToJson n = fromLazyByteString $ encode JsonUnreadCount {unreadCount = n}

--------------------------------------------------------------------------------

retweetToJson :: Either (ApiError String) FeedMessage -> Builder
retweetToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

retweetToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack x}

retweetToJson (Right t) = fromLazyByteString . encode . Ok $ JsonResponse {okTitle="ok", okFeedMessages=[t]}
--------------------------------------------------------------------------------

adhocToJson :: Either (ApiError String) [FeedMessage] -> Builder
adhocToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

adhocToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack x}

adhocToJson (Right ts) = fromLazyByteString . encode . Ok $ JsonResponse {okTitle="ok", okFeedMessages=ts}

--------------------------------------------------------------------------------

starToJson :: Either (ApiError String) FeedMessage -> Builder
starToJson (Left (ApiError msg)) =
    fromLazyByteString . encode . Fail $ JsonApiError {errTitle = "Error", errMessage = T.pack msg}
starToJson (Left (TransportError x)) =
    fromLazyByteString . encode . Fail $ JsonApiError {errTitle = "Error", errMessage = T.pack x}

starToJson (Right t) = fromLazyByteString . encode . Ok $ JsonResponse {okTitle="ok", okFeedMessages=[t]}

--------------------------------------------------------------------------------

tweetToJson :: Either (ApiError String) FeedMessage -> Builder
tweetToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

tweetToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack x}

tweetToJson (Right t) = fromLazyByteString $ encode JsonResponse {okTitle="ok", okFeedMessages=[t]}
