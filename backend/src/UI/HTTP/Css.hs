{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Css where

import           Clay
import           Clay.Animation
import qualified Clay        as C
import           Data.Monoid ((<>))
import           Data.Foldable (foldMap)
import           Prelude hiding (rem)

entityColor = "#2FC2EF" :: Color
baseColor = "#404040" :: Color

animationCss :: Css
animationCss = do
  keyframesFromTo "fadeIn" (opacity 0) (opacity 1)
  keyframesFromTo "fadeOut" (opacity 1) (opacity 0)

  ".animated" ? do
    animationDuration (sec 1)
    "animation-fill-mode" -: "both"

  ".fadeIn" ? do
    "animation-name" -: "fadeIn"

  ".fadeOut" ? do
    "animation-name" -: "fadeOut"

bodyCss :: Css
bodyCss = do
  body ? do
    background  ("#ececec" :: Color)
--    fontFamily  ["Helvetica Neue"] [sansSerif]
    textAlign (alignSide sideCenter)
    margin (px 0) (px 0) (px 0) (px 0)
    color baseColor

    fontFamily ["Roboto"] [sansSerif]
    fontSize (px 16)
    fontWeight $ weight 300

  li ? do
    listStyleType none
    position relative
    margin (px 25) (px 5) (px 25) (px 5)
    clear both
    --verticalAlign textTop

  li # hover ? do
    ".toolbar-target" ? do
        display block

  ul ? do
    padding (px 0) (px 0) (px 0) (px 0)

  a ? do
    color baseColor
    textDecoration none
    --borderBottom solid (px 1) blue

  a # hover ? do
    textDecoration underline

  pre ? do
    display none

  ".video" ? do
    marginTop (px 30)

  ".inline-link" ? do
--    borderBottom solid (px 1) lightblue
    color entityColor
    --width (em 2)
    --display inlineBlock
    --overflow hidden
    --position relative
    --top (px 4)

  --".inline-link" # after ? do
    --content (stringContent "...")

  ".inline-link" # hover ? do
    color entityColor
    --width auto

  --".inline-link" # after # hover ? do
    --content none

  ".username-tag" ? do
    fontWeight (weight 400)
    color entityColor


  ".colon" ? do
    marginRight (px 15)

  ".container" ? do
    width (px 700)
    marginTop (px 20)
    marginBottom (px 20)
    marginLeft (auto)
    marginRight (auto)
    textAlign (alignSide sideLeft)
    background white
    paddingTop (px 10)
    paddingBottom (px 20)
    paddingLeft (px 20)
    paddingRight (px 20)
    boxShadow 0 0 (px 4) (setA 50 baseColor)
    transition "all" (ms 200) linear (ms 200)
    fontSize (px 23)

  ".notification-item" ? do
    width (px 360)
    minHeight (px 80)
    maxHeight (px 300)
    right (px 20)
    zIndex 10000
    position relative
    display flex
    "flex-direction" -: "column"
    "box-shadow" -: "0 2px 5px 0 rgba(0, 0, 0, 0.16), 0 2px 10px 0 rgba(0, 0, 0, 0.12)"
    padding (px 14) (px 11) (px 20) (px 20)
    margin (rem 0.5) (rem 0) (rem 1) (rem 0)
    borderRadius (px 2) (px 2) (px 2) (px 2)
    background white
    transition "all" (ms 400) ease (ms 400)
    "border" -: "1px solid transparent"

  ".notification-item.notification-error" ? do
    "background-color" -: "#f8d7da"
    "border-color" -: "#f5c6cb"
    "font-weight" -: "400"
    ".notification-item-body" ? do
      "color" -: "#721c24"
    ".notification-item-header" ? do
      "color" -: "#721c24"
      "font-weight" -: "400"
    ".close-all-button" ? do
      "color" -: "#721c24"
      "background-color" -: "#f8d7da"
      "border-color" -: "#f5c6cb"

  ".notification-item.notification-warning" ? do
    "background-color" -: "#fff3cd"
    "border-color" -: "#ffeeba"
    "font-weight" -: "400"
    ".notification-item-body" ? do
      "color" -: "#856404"
    ".notification-item-header" ? do
      "color" -: "#856404"
      "font-weight" -: "400"
    ".close-all-button" ? do
      "color" -: "#856404"
      "background-color" -: "#fff3cd"
      "border-color" -: "#ffeeba"

  ".notification-item.notification-success" ? do
    "background-color" -: "#d4edda"
    "border-color" -: "#c3e6cb"
    "font-weight" -: "400"
    ".notification-item-body" ? do
      "color" -: "#155724"
    ".notification-item-header" ? do
      "color" -: "#155724"
      "font-weight" -: "400"
    ".close-all-button" ? do
      "color" -: "#155724"
      "background-color" -: "#d4edda"
      "border-color" -: "#c3e6cb"

  ".notification-item.notification-info" ? do
    "background-color" -: "#cce5ff"
    "border-color" -: "#b8daff"
    "font-weight" -: "400"
    ".notification-item-body" ? do
      "color" -: "#004085"
    ".notification-item-header" ? do
      "color" -: "#004085"
      "font-weight" -: "400"
    ".close-all-button" ? do
      "color" -: "#004085"
      "background-color" -: "#cce5ff"
      "border-color" -: "#b8daff"

  ".notification-item-header" ? do
    textAlign (alignSide sideLeft)
    "margin-bottom" -: "10px"

  ".notification-item-body" ? do
    overflowY auto
    overflowX hidden
    textAlign (alignSide sideLeft)
    fontSize (px 13)

  ".notification-wrapper" ? do
    position fixed
    right (px 0)
    zIndex 999

  ".notification-wrapper.hide" ? do
    display none

  ".notification-icon" ? do
    display flex
    width auto
    position relative
    marginRight (px 10)

  ".notification-icon2" ? do
    marginLeft (px (-20))

  ".notification-body" ? do
    fontSize (px 14)

  ".close-button" ? do
    "cursor" -: "pointer"
    "border-radius" -: "2px"
    position absolute
    fontSize (px 24)
    top (px 5)
    right (px 6)
    color black
    "outline" -: "none"
    "background-color" -: "transparent"
    "border" -: "none"

  ".close-all-button" ? do
    "cursor" -: "pointer"
    "border" -: "1px solid transparent"
    top (px 12)
    position absolute
    right (px 40)

  ".user-icon" ? do
    display inlineBlock
    width (px 30)
    height (px 30)
    marginTop auto
    marginBottom auto
    marginRight (px 20)
    verticalAlign textTop
    position relative
    --top (px 7)

  ".user-icon1" ? do
    display inlineBlock
    width (px 30)
    height (px 30)
    marginTop auto
    marginBottom auto
    marginRight (px 20)
    verticalAlign textTop
    position absolute
    left (px (-10))

    ".user-icon-img" ? do
      borderRight solid (px 1) white

  ".user-icon2" ? do
    display inlineBlock
    width (px 30)
    height (px 30)
    marginTop auto
    marginBottom auto
    marginRight (px 20)
    verticalAlign textTop
    position absolute
    left (px 10)

    "img" # hover ? do
        zIndex 100

  ".user-icon-img" ? do
    width (px 36)
    height (px 36)
    margin (px 0) (px 0) (px 0) (px 0)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    transition "all" (ms 150) ease (ms 150)

  -- ".user-icon-img" # hover ? do
    -- transform $ scale 1.5 1.5

  ".tweet-body" ? do
    width (px 600)
    display inlineBlock
    verticalAlign textTop

  ".no-tweets" ? do
    textAlign (alignSide sideCenter)
--    fontWeight bold

  ".error" ? do
    textAlign (alignSide sideCenter)
    color red
    padding (px 25) (px 25) (px 25) (px 25)
    marginBottom (px (-20))

  ".success" ? do
      textAlign (alignSide sideCenter)
      color green
      padding (px 25) (px 25) (px 25) (px 25)
      marginBottom (px (-20))

  ".no-new-tweets" ? do
    background grey

  ".refresh" ? do
    textAlign (alignSide sideCenter)
    fontWeight bold
    padding (px 40)  (px 40)  (px 40)  (px 40)
    fontSize (px 30)
    marginBottom (px 100)

    ".there-are-new-tweets" ? do
        padding (px 4)  (px 4)  (px 4)  (px 4)
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
        background  orange
        borderTop solid (px 0) red
        borderBottom solid (px 0) red
        borderLeft solid (px 0) red
        borderRight solid (px 0) red
        width (px 70)
        height (px 70)
        color white
        fontWeight bold
        fontSize (px 20)
        cursor pointer
        boxShadow 0 0 (px 4) (setA 50 baseColor)
        transition "all" (ms 150) ease (ms 150)

    ".no-new-tweets" ? do
        padding (px 4)  (px 4)  (px 4)  (px 4)
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
        borderTop solid (px 0) red
        borderBottom solid (px 0) red
        borderLeft solid (px 0) red
        borderRight solid (px 0) red
        width (px 70)
        height (px 70)
        color white
        fontWeight bold
        fontSize (px 20)
        cursor pointer
        boxShadow 0 0 (px 4) (setA 50 baseColor)
        background lightgray
        transition "all" (ms 100) linear (ms 100)

    ".there-are-new-tweets" # focus ? do
        outline solid (px 0) baseColor

    ".no-new-tweets" # focus ? do
        outline solid (px 0) baseColor

    ".there-are-new-tweets" # hover ? do
        background cyan
        transform $ scale 1.15 1.15


    a ? do
      transition "all" (ms 100) linear (ms 100)

    a # hover ? do
      textDecoration none
      textShadow (px 1) (px 1) (px 2) ("#404040" :: Color)

  ".refresh-bottom" ? do
    textAlign (alignSide sideCenter)
    marginTop (px 40)
    fontWeight bold

  ".inline-img" ? do
    marginLeft (px 50)
    marginTop (px 15)
    maxWidth (px 600)

  ".unknown-media" ? do
    padding (px 20) (px 20) (px 20) (px 20)
    background  ("#cccccc" :: Color)
    textAlign (alignSide sideCenter)

  ".unparsable" ? do
    borderTop solid (px 1) red
    borderBottom solid (px 1) red
    borderLeft solid (px 1) red
    borderRight solid (px 1) red

  ".toolbar-target" ? do
    width (px 140)
    height (px 40)
    position absolute
    right (px (-80))
    top (px 0)
    zIndex 50
    display none
    borderTop solid (px 1) transparent
    borderBottom solid (px 1) transparent
    borderLeft solid (px 1) transparent
    borderRight solid (px 1) transparent

  ".toolbar" ? do
    background transparent
    color white
    position absolute
    fontSize (px 28)
    zIndex 100
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)

    li ? do
        padding (px 5) (px 10) (px 5) (px 10)
        display inline
        margin (px 0) (px 0) (px 0) (px 0)
        cursor pointer
        background $ rgba 40 40 40 222

    li # hover ? do
        background red

    a ? do
      color white


  ".toolbar" # hover ? do
      display block


  ".hash-tag" ? do
    color blue

    a ? do
        color baseColor
        fontStyle italic

  "#write-tweet-id" ? do
    display none

  ".history-button" ? do
    background transparent
    border solid (px 1) transparent
    cursor pointer
    color gray
    transition "all" (ms 180) linear (ms 180)
    width (px 25)
    height (px 25)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)

    ".disabled" ? do
        color white
        background red
        border solid (px 1) red

    ".disabled" # hover ? do
        color gray
        background transparent
        border solid (px 1) transparent

  --".history-button" # hover ? do
    --color black
--     boxShadow 0 0 (px 4) (setA 50 baseColor)
    --background lightgreen
    --border solid (px 1) lightgreen

  ".history-button" # focus ? do
    outline solid (px 0) baseColor

  ".remove-message" ? do
    marginLeft (px 5)
    border solid (px 0) transparent
    cursor pointer

  ".write-tweet" ? do
    position fixed
    top (pct 50)
    transform $ translateY (px (-70))
    width (pct 100)
    height (px 50)
    background $ rgba 40 40 40 222
    paddingTop (px 48)
    paddingBottom (px 48)

    input # focus ? do
      borderTop solid (px 1) transparent
      borderBottom solid (px 1) transparent
      borderLeft solid (px 1) transparent
      borderRight solid (px 1) transparent

    input ? do
      width (px 567)
      fontSize (px 25)
--       marginTop (px 15)
      padding (px 7) (px 11) (px 7) (px 11)

  ".writer-button" ? do
    textAlign (alignSide sideCenter)
    fontWeight bold
    padding (px 10)  (px 10)  (px 10)  (px 10)
    fontSize (px 15)
    marginLeft (px 20)
    --borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    background ("#555555" :: Color)
    borderTop solid (px 0) red
    borderBottom solid (px 0) red
    borderLeft solid (px 0) red
    borderRight solid (px 0) red
    width (px 42)
    height (px 42)
    position relative
    top (px (-2))
    color white
    cursor pointer
    --boxShadow 0 0 (px 4) (setA 50 baseColor)
    transition "all" (ms 150) ease (ms 150)

  ".ok" # hover ?
    background green

  ".nok" # hover ?
    background red

  ".red" ? do
    color red

  ".green" ? do
    color green

  ".blue" ? do
    color blue

  ".popup-panel-label" ? do
    fontSize (px 36)

  ".tweet-toolbar" ? do
    position absolute
    top (px 0)
    right (px (-30))
    width (px 34)
    height (px 125)
    background white
    display none
    borderRadius (px 17) (px 17) (px 17) (px 17)
    textAlign (alignSide sideCenter)

  ".tweet-toolbar-a" ? do
    position absolute
    top (px (-31))
    left (px (-5))
    width (px 62)
    height (px 32)
    background white
    display none
    borderRadius (px 17) (px 17) (px 17) (px 17)
    textAlign (alignSide sideCenter)

  ".tweet-toolbar-b" ? do
    position absolute
    top (px (-31))
    left (px (-5))
    width (px 62)
    height (px 32)
    background white
    display none
    borderRadius (px 17) (px 17) (px 17) (px 17)
    textAlign (alignSide sideCenter)

  ".panel" # hover ? do
    ".tweet-toolbar" ? do
      display block

  ".user-icon-x" # hover ? do
    ".tweet-toolbar-a" ? do
      display block

  ".user-icon-y" # hover ? do
    ".tweet-toolbar-b" ? do
      display block

  ".tweet-toolbar-button" # hover ? do
    background black
    color white

  ".tweet-toolbar-button" ? do
    color black
    background $ rgba 222 222 222 222
    border solid (px 0) transparent
    cursor pointer
    fontSize (px 11)
    padding (px 2) (px 6) (px 3) (px 6)
    textDecoration none
    width (px 25)
    height (px 25)
    "border-radius" -: "50%"


  ".busy-wrapper" ? do
    position fixed
    bottom (px 10)
    right (px 10)

  ".thread-block" ? do
    border solid (px 0) red

  ".embedded-tweet" ? do
    background white
    border solid (px 0) transparent
    borderRadius (px 3) (px 3) (px 3) (px 3)
    -- "box-shadow" -: "0 0 1px 0 rgba(0,0,0,0.5), 0 1px 10px 0 rgba(0,0,0,0.15) "
    margin (px 0) (px 0) (px 0) (px 0)
    maxWidth (px 600)
    padding (px 0) (px 0) (px 0) (px 0)
    width  (px 600)
    marginLeft (px 50)
    marginTop (px 30)
    position relative
    borderLeft  solid (px 1) ("#cccccc" :: Color)
    borderRight solid (px 1) ("#cccccc" :: Color)

    ".tweet-toolbar" ? do
      right (px (-24))

    ".tweet-toolbar-a" ? do
      left (px (-24))

    ".embedded-tweet" ? do
      ".tweet-body" ? do
        width (px 400)

    ".tweet-body" ? do
      width (px 500)

    ".inline-img" ? do
      maxWidth (px 500)

    ".youtube" ? do
      maxWidth (px 500)
      width (px 500)
      height auto

usernameCss :: Css
usernameCss = ".user-name" ? do
  --fontWeight bold
  width (px 180)
  display none
  verticalAlign textTop
  position relative
  top (px 6)
  position absolute

  a # hover ? do
    borderBottom solid (px 1) blue

  ".user-name" # hover ? do
    borderBottom solid (px 1) blue


allCss =  foldMap render [animationCss, bodyCss, usernameCss]
