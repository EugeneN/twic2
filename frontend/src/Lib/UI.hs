{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}

module Lib.UI where

import Prelude
import Control.Applicative           ((<*>), (<$>))

import Data.Map.Strict               (Map)
import qualified Data.Map.Strict     as Map
import Data.Monoid
import qualified Data.Text           as T

import qualified Data.VirtualDOM     as VD

import qualified Data.JSString      as JSS
import           GHCJS.Prim         (JSVal)

import qualified BL.Types           as BL
import  BL.Instances


setTitle :: String -> IO ()
setTitle = js_setTitle . JSS.pack

foreign import javascript unsafe "document.title = $1"
    js_setTitle :: JSS.JSString -> IO ()

foreign import javascript unsafe "window.scrollTo(0,0)"
    scrollToTop :: IO ()

newtype Attrs = A { unA :: [(String, String)]}

p (A x) = VD.prop x
p_ x    = VD.prop x

instance Monoid Attrs where
  mempty = A []
  mappend (A a) (A b) = A . Map.toList $ Map.unionWithKey f (Map.fromList a) (Map.fromList b)
    where
      f k x y = case k of
        "class" -> x <> " " <> y
        "style" -> x <> ";" <> y
        other   -> x <> " " <> y

instance Monoid (VD.VNode l) where
  mempty      = VD.text ""
  mconcat as  = VD.h "div" (VD.prop []) as
  mappend a b = VD.h "div" (VD.prop []) [a, b]

redButton   = A [("style", "background-color: red;   color: white; padding: 10px;")]
flatButton  = A [("style", "background-color: transparent; color: grey; padding: 10px; border: none; cursor: pointer;")]
greenButton = A [("style", "background-color: green; color: white; padding: 10px;")]
blueButton  = A [("style", "background-color: blue;  color: white; padding: 10px;")]
greyButton  = A [("style", "background-color: #c0c0c0;  color: darkgrey; padding: 10px;")]
roundButton = A [("style", "border-radius: 50%")]

block          = VD.h "div"  (p_ [])
block_ cls xs  = VD.h "div"  (p_ [("class", cls)])                          xs
textLabel t    = VD.h "span" (p_ [("style", "padding: 10px;")])             [VD.text t]
textLabel_ cls t = VD.h "span" (p_ [("class", cls)])                        [VD.text t]
errorLabel t   = VD.h "span" (p_ [("style", "padding: 10px; color: red;")]) [VD.text t]
inlineLabel t  = VD.h "span" (p_ [("style", "padding: 0px;")])              [VD.text $ T.unpack t]
inlineLabel_ v = VD.h "span" (p_ [("style", "padding: 0px;")])              [v]
link h t       = VD.h "a"    (p_ [("href", T.unpack h), ("target", "_blank")])                 [VD.text $ T.unpack t]
link' cls h t  = VD.h "a"    (p_ [("class", cls), ("href", T.unpack h), ("target", "_blank")]) [VD.text $ T.unpack t]
link_ h v      = VD.h "a"    (p_ [("href", T.unpack h), ("target", "_blank")])                 [v]

button label attrs listeners =
  flip VD.with listeners $
    VD.h "button" attrs [VD.text label]

foreign import javascript unsafe "$1.target.value"
  jsval :: JSVal -> JSS.JSString

stringInput u =
  flip VD.with [VD.On "change" (\ev -> u . JSS.unpack . jsval $ ev)] $
    VD.h "input" (VD.prop [("type", "text"), ("style", "padding: 2px 5px; margin: 5px 0px;")]) []

noTweetsLabel = VD.h "div" (p_ [("class", "no-tweets")]) . (:[]) . VD.text
container = VD.h "div" (VD.prop [("id", "container"), ("class", "container")])

panel ch = VD.h "div"
                (VD.prop [ ("class", "panel")
                         , ("style", "padding: 10px; border: 1px solid grey; width: auto; display: inline-block; margin: 5px;")])
                ch
panel' ch = VD.h "div"
                (VD.prop [ ("class", "panel")
                         , ("style", "padding: 10px; border: 0px solid grey; width: auto; display: inline-block; margin: 5px;")])
                ch

list xs = VD.h "ul"
               (VD.prop [ ("class", "list")
                        , ("style", "text-align: left;")])
               (fmap listItem xs)

listItem x = VD.h "li"
               (VD.prop [ ("class", "list-ietm")
                        , ("style", "")])
               [x]

columns cs =
  VD.h "div" (VD.prop [("style", "display: flex; flex-direction: row; flex-wrap: nowrap ; justify-content: flex-start; align-items: stretch;")])
       (fmap (\(x, pctWidth) -> VD.h "div" (VD.prop [("style", "align-self: stretch; flex-basis: " <> show pctWidth <> "%;")]) [x]) cs)


-- Events (should be Monoid?)

onClick = VD.On "click"
onClick_ f = VD.On "click" (\_ -> f >> pure ())
-- onChange_ f = VD.On "change" (pure () << f . JSS.unpack . jsval)