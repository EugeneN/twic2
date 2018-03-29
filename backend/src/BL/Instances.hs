{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module BL.Instances where

import           Control.Applicative ((<|>))
import           Data.Aeson

import           BL.Types
import           Control.Monad
import           Data.Maybe          (Maybe (..), fromMaybe, catMaybes)
import           Data.Text           (Text)
#ifndef __GHCJS__
import           BL.Parser           (parseTweet)
import           BL.Utils
#endif


#ifndef __GHCJS__
--   see <https://dev.twitter.com/docs/platform-objects/tweets>.
-- this parses tweet json returned from twitter api
instance FromJSON Tweet where
  parseJSON (Object x) = do
    -- text        <- x .: "text"
    text        <- ((x .: "extended_tweet" >>= (.: "full_text")) <|> x .: "text")
    created_at  <- x .: "created_at"
    id_         <- x .: "id"
    id_str      <- x .: "id_str"
    user        <- x .: "user"
    entities    <- x .: "entities"
    extendedEntities <- x .:? "extended_entities"
    retweet     <- x .:? "retweeted_status"
    status_favorited   <- x .:? "favorited"
    status_retweeted   <- x .:? "retweeted"
    statusInReplyToStatusId   <- x.:? "in_reply_to_status_id"
    statusInReplyToUserId     <- x.:? "in_reply_to_user_id"
    statusInReplyToScreenName <- x.:? "in_reply_to_screen_name"

    return $ Tweet (parseTweet text)
                   created_at
                   id_
                   id_str
                   user
                   entities
                   (fromMaybe (Entities [] [] Nothing) extendedEntities)
                   retweet
                   status_favorited
                   status_retweeted
                   statusInReplyToStatusId
                   statusInReplyToUserId
                   statusInReplyToScreenName

  parseJSON _ = fail "tweet is expected to be an object"

#else

-- this parses tweet json used between the backend and the frontend
instance FromJSON Tweet

#endif
instance ToJSON Tweet

instance FromJSON TASettings where
  parseJSON (Object x) = do
    sn <- x .: "screen_name"
    return TASettings { accScreenName = sn }

  parseJSON _ = fail "Account Settings is expected to be an object"

instance ToJSON TASettings where
  toJSON x = object [ "screen_name" .= accScreenName x ]

instance FromJSON JsonApiError
instance ToJSON JsonApiError

instance FromJSON FeedMessage
instance ToJSON FeedMessage

instance FromJSON JsonResponse
instance ToJSON JsonResponse

instance FromJSON TheResponse
instance ToJSON TheResponse

instance FromJSON JsonUserInfo
instance ToJSON JsonUserInfo

instance FromJSON LoginInfo
instance ToJSON LoginInfo

-- instance FromJSON User
-- instance ToJSON User


#ifndef __GHCJS__
instance FromJSON BL.Types.Entities where
  parseJSON (Object x) = do
    urls <- x .:? "urls"
    hts <- x .:? "hashtags"
    ms <- x .:? "media"

    return $ BL.Types.Entities (fromMaybe [] urls) (fromMaybe [] hts) ms

  parseJSON _ = fail "entities is expected to be an object"

#else
instance FromJSON BL.Types.Entities
#endif
instance ToJSON BL.Types.Entities

#ifndef __GHCJS__
instance FromJSON EntityUrl where
  parseJSON (Object x) = EntityUrl <$> x .: "expanded_url"
                                   <*> x .: "url"
                                   <*> x .: "indices"
                                   <*> x .: "display_url"
  parseJSON _ = fail "entity url is expected to be an object"

#else

instance FromJSON EntityUrl

#endif


instance ToJSON EntityUrl

#ifndef __GHCJS__
instance FromJSON EntityHashtag where
  parseJSON (Object x) = EntityHashtag <$> x .: "text" <*> x .: "indices"
  parseJSON _ = fail "entity hashtag is expected to be an object"

#else
instance FromJSON EntityHashtag
#endif

instance ToJSON EntityHashtag

#ifndef __GHCJS__
instance FromJSON EntityMedia where
  parseJSON (Object x) = EntityMedia <$> x .: "type"
                                     <*> x .: "indices"
                                     <*> x .: "url"
                                     <*> x .: "media_url"
                                     <*> x .: "display_url"
                                     <*> x .: "expanded_url"
                                     <*> x .: "sizes"
  parseJSON _ = fail "entity media is expected to be an object"
#else
instance FromJSON EntityMedia
#endif
instance ToJSON EntityMedia

instance FromJSON EntityMediaSize
instance ToJSON EntityMediaSize

instance FromJSON EntityMediaSizes
instance ToJSON EntityMediaSizes

#ifndef __GHCJS__
instance FromJSON Author where
  parseJSON (Object x) = Author <$> x .: "name"
                                <*> x .: "id"
                                <*> x .: "screen_name"
                                <*> x .: "default_profile_image"
                                <*> x .: "profile_image_url"
  parseJSON _ = fail "Author is expected to be an object"

#else
instance FromJSON Author
#endif

instance ToJSON Author
instance FromJSON JsonUnreadCount
instance ToJSON JsonUnreadCount

toTweetToken :: String -> Text -> TweetElement
toTweetToken "AtUsername"   = AtUsername
toTweetToken "Link"         = Link
toTweetToken "PlainText"    = PlainText
toTweetToken "Hashtag"      = Hashtag
toTweetToken "Spaces"       = Spaces
toTweetToken "Unparsable"   = Unparsable

#ifndef __GHCJS__

instance FromJSON TweetElement where
    parseJSON (Object x) = toTweetToken <$> x .: "type" <*> x .: "value"
    parseJSON _ = mzero

#else
instance FromJSON TweetElement
#endif

instance ToJSON TweetElement

#ifndef __GHCJS__
instance FromJSON Cfg where
  parseJSON (Object x) = do
    cfgOauthConsumerKey <- x .: "oauthConsumerKey"
    cfgOauthConsumerSecret <- x .: "oauthConsumerSecret"
    cfgAccessToken <- x .:? "accessToken"
    cfgAccessTokenSecret <- x .:? "accessTokenSecret"
    cfgCloudDbUrl <- x .: "cloudDbUrl"

    return $ Cfg {..}
    
  parseJSON _ = mzero

instance ToJSON Cfg where
  toJSON (Cfg {..}) = object $ catMaybes [ "oauthConsumerKey" .== cfgOauthConsumerKey
                                         , "oauthConsumerSecret" .== cfgOauthConsumerSecret
                                         , "accessToken" .=? cfgAccessToken
                                         , "accessTokenSecret" .=? cfgAccessTokenSecret
                                         , "cloudDbUrl" .== cfgCloudDbUrl]

instance FromJSON AccessCfg where
  parseJSON (Object x) = do
    acfgAccessToken <- x .:? "accessToken"
    acfgAccessTokenSecret <- x .:? "accessTokenSecret"

    return $ AccessCfg {..}
    
  parseJSON _ = mzero

instance ToJSON AccessCfg where
  toJSON (AccessCfg {..}) = object $ catMaybes [ "accessToken" .=? acfgAccessToken
                                                , "accessTokenSecret" .=? acfgAccessTokenSecret]                                           

#endif                                                