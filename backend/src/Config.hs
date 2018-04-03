module Config where

serverName          = "api.twitter.com"

accessConfig        = "twic.json"
logFile             = "twic.log"

cloudDbUrl          = "https://twic2-7c5e4.firebaseio.com/.json"
oauthConsumerSecret = "AEJJeCyV5b3yKYoX8p1MEqJkbjOsx7vSP9AlVjWgj6QBwNWcFb"
oauthConsumerKey    = "3zUlEZ2bSbRLvRFL3Tvesb8tK"

oneSecond           = 1000000 :: Int
oneMinute           = 60 * oneSecond

port                = 3000 :: Int
delay               = 2 * oneMinute

heartbeatDelay      = 15 :: Int -- seconds

timeoutThreshod     = 90
timeoutWorkerDelay  = (timeoutThreshod * oneSecond) - oneSecond

updateRetryCount    = 30 :: Int
updateRetryDelay    = 2 * oneSecond
updateFeedAsync     = False
