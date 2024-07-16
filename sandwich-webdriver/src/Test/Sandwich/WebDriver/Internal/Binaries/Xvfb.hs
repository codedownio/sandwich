{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Xvfb (
  obtainXvfb

  -- * Types
  , XvfbDependenciesSpec(..)
  , XvfbToUse(..)
  , FluxboxToUse(..)
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb.Types
import UnliftIO.Directory


-- | Manually obtain an Xvfb binary, according to the 'XvfbToUse' policy.
obtainXvfb :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  ) => XvfbToUse -> m (Either T.Text FilePath)
obtainXvfb UseXvfbFromPath = findExecutable "xvfb" >>= \case
  Nothing -> return $ Left [i|Couldn't find "xvfb" on the PATH.|]
  Just p -> return $ Right p
obtainXvfb (UseXvfbAt path) = doesFileExist path >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> return $ Right path
obtainXvfb (UseXvfbFromNixpkgs nixContext) =
  -- Note: on master *after* release-24.05, there seems to be xorg.xvfb
  Right <$> getBinaryViaNixPackage' @"Xvfb" nixContext "xorg.xorgserver"
