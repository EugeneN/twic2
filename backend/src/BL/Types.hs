{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module BL.Types where

import           Prelude                 hiding (id)

#ifndef __GHCJS__

import           Control.Applicative
import           Control.Concurrent      (MVar, ThreadId)
import           Control.Exception.Base
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.Configurator
import           Data.Configurator.Types
import           Network.HTTP.Conduit

#endif

import           Data.ByteString
import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime (..))
import           GHC.Generics
import           Web.Twitter.Types       (User (..))

type Url = String
type Username = String
type ScreenName = Text
type TweetId = Integer
type AuthorId = Integer
type TweetBody = ByteString

data Message = Message Int

#ifndef __GHCJS__

data IPCMessage = MReloadFeed
                | MExit
                | MRestart
                | MOther
                | MNOOP deriving Show

instance Show (MVar IPCMessage)
instance Show (MVar [FeedMessage])
instance Show (MVar UTCTime)
#endif

data TASettings = TASettings { accScreenName :: ScreenName
                             } deriving (Show, Generic)

data FeedMessage = TweetMessage Tweet
                 | UserMessage User
                 | SettingsMessage TASettings
                 | FriendsListMessage [User]
                 | ErrorMessage JsonApiError
                 deriving (Show, Generic)

type FeedState = [FeedMessage]

type UpdateMessage = UTCTime

#ifndef __GHCJS__

makeAppState :: UTCTime -> a
             -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId
             -> MVar FeedState -> MVar IPCMessage -> MVar UpdateMessage -> MVar UpdateMessage
             -> Cfg
             -> AppState a
makeAppState a b c d e f g h j i k l =
    RunState { startTime        = a
             , db               = b
             , timeoutWorkerId  = c
             , streamWorkerId   = d
             , uiWorkerId       = e
             , updateWorkerId   = f
             , accFetchWorkerId = g
             , feedVar          = h
             , appBusVar        = j
             , updateVar        = i
             , fetchAccountVar  = k
             , conf             = l
             }

-- deriving instance Show Config

data Cfg = Cfg { cfgOauthConsumerKey    :: String
               , cfgOauthConsumerSecret :: String
               , cfgAccessToken         :: String
               , cfgAccessTokenSecret   :: String
               , cfgCloudDbUrl          :: String } deriving Show

data AppState a = RunState { startTime        :: UTCTime
                           , db               :: a
                           , timeoutWorkerId  :: Maybe ThreadId
                           , streamWorkerId   :: Maybe ThreadId
                           , uiWorkerId       :: Maybe ThreadId
                           , updateWorkerId   :: Maybe ThreadId
                           , accFetchWorkerId :: Maybe ThreadId
                           , feedVar          :: MVar [FeedMessage]
                           , appBusVar        :: MVar IPCMessage
                           , updateVar        :: MVar UpdateMessage
                           , fetchAccountVar  :: MVar UpdateMessage
                           , conf             :: Cfg
                           } deriving Show

#endif

data Feed = UserTimeline Url
          | HomeTimeline Url
          | AdhocTweet Url
          deriving Show

data TweetElement = AtUsername Text
                  | Link Text
                  | PlainText Text
                  | Hashtag Text
                  | Retweet
                  | Spaces Text
                  | Unparsable Text
                  | LineBreak
                  deriving (Show, Generic)

data Tweet = Tweet { text                      :: [TweetElement]
                   , created_at                :: Text
                   , id                        :: TweetId
                   , id_str                    :: String
                   , user                      :: Author
                   , entities                  :: Entities
                   , extendedEntities          :: Entities
                   , retweet                   :: Maybe Tweet
                   , status_favorited          :: Maybe Bool
                   , status_retweeted          :: Maybe Bool
                   , statusInReplyToStatusId   :: Maybe TweetId
                   , statusInReplyToUserId     :: Maybe AuthorId
                   , statusInReplyToScreenName :: Maybe Text
                   } deriving (Show, Generic)

data Entities = Entities { urls     :: [EntityUrl]
                         , hashtags :: [EntityHashtag]
                         , media    :: Maybe [EntityMedia]
                         } deriving (Show, Generic)

data EntityUrl = EntityUrl { eExpandedUrl :: Url
                           , eUrl         :: Url
                           , eIndices     :: [Int]
                           , eDisplayUrl  :: String
                           } deriving (Show, Generic)

data EntityHashtag = EntityHashtag { hText    :: Text
                                   , hIndices :: [Int]
                                   } deriving (Show, Generic)

data EntityMedia = EntityMedia { mType        :: String
                               , mIndices     :: [Int]
                               , mUrl         :: Url
                               , mMediaUrl    :: Url
                               , mDisplayUrl  :: String
                               , mExpandedUrl :: Url
                               , mSizes       :: EntityMediaSizes
                               } deriving (Show, Generic)

data EntityMediaSize = EntityMediaSize { h      :: Int
                                       , w      :: Int
                                       , resize :: String
                                       } deriving (Show, Generic)

data EntityMediaSizes = EntityMediaSizes { thumb  :: EntityMediaSize
                                         , large  :: EntityMediaSize
                                         , medium :: EntityMediaSize
                                         , small  :: EntityMediaSize
                                         } deriving (Show, Generic)

data Author = Author { name                  :: Text
                     , authorId              :: Integer
                     , screen_name           :: Text
                     , default_profile_image :: Bool
                     , profile_image_url     :: Url
                     } deriving (Show, Eq, Ord, Generic)

data JsonApiError = JsonApiError { errTitle   :: Text
                                 , errMessage :: Text
                                 } deriving (Show, Generic)

data TheResponse = Ok JsonResponse | Fail JsonApiError deriving (Show, Generic)

data JsonResponse = JsonResponse { okTitle        :: Text
                                 , okFeedMessages :: FeedState
                                 } deriving (Show, Generic)

data JsonUserInfo = JsonUserInfo { uiTitle :: Text
                                 , uiData  :: User
                                 } deriving (Show, Generic)

data LoginInfo = NeedAuth { t :: Text } | NotNeedAuth deriving (Show, Generic)                                 

data Show a => ApiError a = ApiError String | TransportError a deriving Show

instance Eq Tweet where
  Tweet {id = aid} == Tweet {id = bid} = aid == bid

instance Ord Tweet where
   max x@(Tweet {id = aid}) y@(Tweet {id = bid}) = if aid >= bid then x else y
   Tweet {id = aid} <= Tweet {id = bid} = aid <= bid

data JsonUnreadCount = JsonUnreadCount  { unreadCount :: Int } deriving (Show, Generic)
