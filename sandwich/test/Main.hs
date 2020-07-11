{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Writer
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import System.Exit
import Test.Sandwich
import Test.Sandwich.Internal

import qualified Around as Around
import qualified Before as Before
import qualified Describe as Describe
import qualified Introduce as Introduce
import TestUtil


main = mainWith $ do
  Around.tests
  Before.tests
  Describe.tests
  Introduce.tests
