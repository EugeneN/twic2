module BL.Utils where

import Data.Aeson    
import Data.Aeson.Types

import Data.Text

(.==) :: (ToJSON a, Applicative f) => Text -> a -> f Pair
(.==) text value = pure $ text .= value

(.=?) :: (ToJSON a, Applicative f) => Text -> f a -> f Pair
(.=?) text value = (text .=) <$> value