{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Prelude
import App                          (theApp)
import Lib.FW                       (hostApp, appContainer)
-- import Components.Counter (counterComponent)


main = hostApp appContainer theApp
-- main = hostApp appContainer (counterComponent 1 R.never)
