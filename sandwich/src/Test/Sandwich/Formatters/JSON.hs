{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

-- | The print formatter prints all results from the test tree from top to bottom, as they become available.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/formatters/print here>.

module Test.Sandwich.Formatters.JSON (
  defaultJsonFormatter
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Data.Time
import Data.Time.Clock
import System.IO
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util


instance Formatter JsonFormatter where
  formatterName _ = "json-formatter"
  runFormatter = runApp
  finalizeFormatter _ _ _ = return ()

data JsonFormatter = JsonFormatter {
  jsonFormatterRefreshPeriod :: Int
  -- ^ Time in microseconds between messages. Defaults to 100ms.
  }

defaultJsonFormatter = JsonFormatter {
  jsonFormatterRefreshPeriod = 100000
  }

runApp :: (MonadIO m, MonadLogger m) => JsonFormatter -> [RunNode BaseContext] -> Maybe (CommandLineOptions ()) -> BaseContext -> m ()
runApp pf@(JsonFormatter {..}) rts _maybeCommandLineOptions bc = liftIO $ do
  let total = countWhere isItBlock rts


  initialTree <- atomically $ mapM fixRunTree rts

  BL.putStrLn $ A.encode initialTree

  currentFixedTree <- newTVarIO initialTree
  forever $ do
    newFixedTree <- atomically $ do
      currentFixed <- readTVar currentFixedTree
      newFixed <- mapM fixRunTree rts
      when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
      writeTVar currentFixedTree newFixed
      return newFixed

    BL.putStrLn $ A.encode newFixedTree

    threadDelay jsonFormatterRefreshPeriod
