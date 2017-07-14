{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Data.Monoid ((<>))
import BL.Types (Tweet(..), Author(..), Entities(..), TweetElement(..))


main =
  let t = Tweet  { text       = [PlainText "ceci n'est pas un tweet"]
                 , created_at = "?"
                 , id_        = 123
                 , id_str     = "???"
                 , user       = Author { name                  = "Text"
                                       , authorId              = 123
                                       , screen_name           = "Text"
                                       , default_profile_image = False
                                       , profile_image_url     = "???" }
                 , entities   = Entities { urls     = []
                                         , hashtags = []
                                         , media    = Nothing }
                 , retweet    = Nothing
                 , status_favorited = Nothing
                 , status_retweeted = Nothing
                 }
  in print $ "hello" <> show t
