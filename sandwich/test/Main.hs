{-# LANGUAGE DataKinds #-}

module Main where

import qualified Around
import qualified Before
import qualified Describe
import qualified Introduce
import TestUtil


main = mainWith $ do
  Around.tests
  Before.tests
  Describe.tests
  Introduce.tests
