{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import Prelude
import Data.Monoid ((<>))
import qualified Data.Text as T
import BL.Types (Tweet(..), Author(..), Entities(..), TweetElement(..))

import qualified Reflex.Dom as RD
import qualified Reflex     as R
-- import  Reflex.Class (zipDynWith, zipDyn)
import qualified Data.Map   as Map
import qualified Safe       as S
import Control.Applicative ((<*>), (<$>))

main = main1


main0 =
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
  in RD.mainWidget $ RD.el "div" $ RD.text $ T.pack $ "Welcome to Reflex" <> show t

main1 = RD.mainWidget $ RD.el "div" $ do
  nx <- numberInput
  d  <- RD.dropdown "*" (R.constDyn ops) RD.def
  ny <- numberInput

  let values = R.zipDynWith (,) nx ny
      result = R.zipDynWith (\o (x,y) -> textToOp o <$> x <*> y) (RD._dropdown_value d) values
      resultText = fmap (T.pack . show) result

  RD.text " = "
  numberLabel result

  pure ()

numberLabel :: (RD.MonadWidget t m, Num a, Ord a, Show a) => R.Dynamic t (Maybe a) -> m ()
numberLabel x = do
  RD.elDynAttr "span" (fmap valToAttr x) $
    RD.dynText (fmap (T.pack . show) x)

  where
    negState  = "style" RD.=: "color: red"
    posState  = "style" RD.=: "color: green"
    zeroState = "style" RD.=: "color: blue"
    nanState  = "style" RD.=: "color: white; background-color: red;"

    valToAttr v = case v of
      Nothing            -> nanState
      Just x | x > 0     -> posState
             | x == 0    -> zeroState
             | otherwise -> negState

numberInput :: (RD.MonadWidget t m) => m (R.Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" RD.=: "border-color: red"
      validState = "style" RD.=: "border-color: green"
  rec n <- RD.textInput $ RD.def RD.& RD.textInputConfig_inputType    RD..~ "number"
                                 RD.& RD.textInputConfig_initialValue RD..~ "0"
                                 RD.& RD.textInputConfig_attributes   RD..~ attrs
      let result = fmap (S.readMay . T.unpack) $ RD._textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

textToOp :: (Fractional a) => T.Text -> a -> a -> a
textToOp s = case s of
  "-" -> (-)
  "*" -> (*)
  "/" -> (/)
  _   -> (+)
