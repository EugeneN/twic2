{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Prelude
import App                          (theApp)
import Lib.FW                       (hostApp, appContainer)
-- import Components.Counter (counterApp)


main = hostApp appContainer theApp
-- main = hostApp appContainer (counterApp 1 R.never)
