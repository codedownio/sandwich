{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Sandwich.Contexts.Nix.MinIO (
  minio
  , introduceMinio

  , MinioNixOptions
  , defaultMinioNixOptions

  , MinioContext(..)
  ) where

import Sandwich.Contexts.Nix
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Relude hiding (withFile)
import Test.Sandwich

-- * Labels

minio :: Label "minio" MinioContext
minio = Label

-- * Types

data MinioNixOptions = MinioNixOptions {

  }
defaultMinioNixOptions :: MinioNixOptions
defaultMinioNixOptions = MinioNixOptions {
  }

-- TODO: use the same context here as the container version
data MinioContext = MinioContext {
  } deriving (Show)


introduceMinio :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => MinioNixOptions -> SpecFree (LabelValue "minio" MinioContext :> context) m () -> SpecFree context m ()
introduceMinio (MinioNixOptions {}) = introduceWith "Minio via Nix" minio $ \_action -> do
  undefined
