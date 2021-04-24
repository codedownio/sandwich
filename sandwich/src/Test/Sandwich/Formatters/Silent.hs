{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sandwich.Formatters.Silent (
  defaultSilentFormatter
  , silentFormatterPrintRunRoot
  ) where

import Control.Monad.IO.Class
import Data.String.Interpolate
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Util


data SilentFormatter = SilentFormatter {
  silentFormatterPrintRunRoot :: Bool
  } deriving (Show)

defaultSilentFormatter :: SilentFormatter
defaultSilentFormatter = SilentFormatter {
  silentFormatterPrintRunRoot = True
  }

instance Formatter SilentFormatter where
  formatterName _ = "silent-formatter"
  runFormatter _ _ _ bc =
    whenJust (baseContextRunRoot bc) $ \runRoot ->
      liftIO $ putStrLn [i|Run root: #{runRoot}\n|]
  finalizeFormatter _ _ _ = return ()
