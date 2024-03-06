{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Files (
  introduceBinaryViaEnvironment
  , introduceBinaryViaNix
  , introduceBinaryViaNixProxy'
  , introduceBinaryViaNixProxy''

  , askFile
  , askFileProxy

  , EnvironmentFile(..)
  , HasFile
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import GHC.TypeLits
import Relude
import Sandwich.Contexts.Nix
import System.FilePath
import Test.Sandwich
import UnliftIO.Directory


-- | A file path to make available to tests.
-- For example, this can be an external binary like "minikube" if a given test context wants
-- to use it to start a Minikube cluster.
-- But you can use this for any kind of file you want to inject into tests.
data EnvironmentFile a = EnvironmentFile { unEnvironmentFile :: FilePath }

-- | Has-* class for asserting a given file is available.
type HasFile context a = HasLabel context (AppendSymbol "file-" a) (EnvironmentFile a)

mkLabel :: Label (AppendSymbol "file-" a) (EnvironmentFile a)
mkLabel = Label

askFile :: forall a context m. (MonadReader context m, HasFile context a) => m FilePath
askFile = askFileProxy (Proxy @a)

askFileProxy :: forall a context m. (MonadReader context m, HasFile context a) => Proxy a -> m FilePath
askFileProxy _ = unEnvironmentFile <$> getContext (mkLabel @a)

-- | Introduce a given 'EnvironmentFile' from the PATH present when tests are run.
-- Useful when you want to set up your own environment with binaries etc. to use in tests.
-- Throws an exception if the desired file is not available.
introduceBinaryViaEnvironment :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  ) => Proxy a -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m () -> SpecFree context m ()
introduceBinaryViaEnvironment proxy = introduce [i|#{symbolVal proxy} (binary from PATH)|] (mkLabel @a) alloc cleanup
  where
    alloc = do
      liftIO (findExecutable (symbolVal proxy)) >>= \case
        Nothing -> expectationFailure [i|Couldn't find binary '#{symbolVal proxy}' on PATH|]
        Just path -> return $ EnvironmentFile path

    cleanup _ = return ()

type NixPackageName = Text

-- | Introduce a given 'EnvironmentFile' from the PATH present when tests are run.
-- Useful when you want to set up your own environment with binaries etc. to use in tests.
-- Throws an exception if the desired file is not available.
introduceBinaryViaNix :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix package name within the configured Nixpkgs version
    NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNix = introduceBinaryViaNixProxy (Proxy @a)

-- | Introduce a given 'EnvironmentFile' from the PATH present when tests are run.
-- Useful when you want to set up your own environment with binaries etc. to use in tests.
-- Throws an exception if the desired file is not available.
introduceBinaryViaNixProxy :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix package name within the configured Nixpkgs version
    -> NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixProxy proxy packageName = introduce [i|#{symbolVal proxy} (binary via Nix)|] (mkLabel @a) alloc (const $ return ())
  where
    alloc = buildNixEnvironment [packageName] >>= tryFindBinary (symbolVal proxy)

-- | Same as 'introduceBinaryViaNix', but using an arbitrary derivation.
introduceBinaryViaNixProxy' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixProxy' = introduceBinaryViaNixProxy'' (Proxy @a)

introduceBinaryViaNixProxy'' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix derivation as a string.
    -> Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixProxy'' proxy derivation = introduce [i|#{symbolVal proxy} (binary via Nix)|] (mkLabel @a) alloc (const $ return ())
  where
    alloc = buildNixExpression derivation >>= tryFindBinary (symbolVal proxy)

tryFindBinary :: (MonadLoggerIO m) => String -> FilePath -> m (EnvironmentFile a)
tryFindBinary binaryName env = do
  findExecutablesInDirectories [env </> "bin"] binaryName >>= \case
    (x:_) -> do
      info [i|Found binary: #{x}|]
      return $ EnvironmentFile x
    _ -> expectationFailure [i|Couldn't find binary '#{binaryName}' in #{env </> "bin"}|]
