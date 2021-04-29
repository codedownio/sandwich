{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The silent formatter does nothing except print the test root folder path, if present.
--
-- This is provided as an explicit formatter so it can print that single line. If you don't want anything at all to be printed, you can just run with no formatters.

module Test.Sandwich.Formatters.Silent (
  defaultSilentFormatter

  -- * Options
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
