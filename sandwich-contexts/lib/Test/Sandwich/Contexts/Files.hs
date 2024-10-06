{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module contains tools for introducing files and making them available to tests. It uses type-level strings, and is mostly intended to be used with -XTypeApplications.

For example:

@
introduceFile \@"grep" "\/path\/to\/grep" $ do
  it "uses grep for something" $ do
    grep <- askFile \@"grep"
    results <- readCreateProcess (proc grep ["foo"]) ""
    todo -- Do something with results
@

For reproducibility, you can leverage a 'NixContext' that's already been introduced to introduce binaries, either by specifying a Nixpkgs package name or by writing out a full derivation.

-}

module Test.Sandwich.Contexts.Files (
  -- * Introduce a file directly
  introduceFile
  , introduceFile'

  -- * Introduce a binary from the environment
  , introduceBinaryViaEnvironment
  , introduceBinaryViaEnvironment'

  -- * Introduce a binary from a Nix package
  , introduceBinaryViaNixPackage
  , introduceBinaryViaNixPackage'
  , getBinaryViaNixPackage
  , getBinaryViaNixPackage'

  -- * Introduce file from a Nix package
  , introduceFileViaNixPackage
  , introduceFileViaNixPackage'
  , introduceFileViaNixPackage''
  , getFileViaNixPackage

  -- * Introduce a binary from a Nix derivation
  , introduceBinaryViaNixDerivation
  , introduceBinaryViaNixDerivation'
  , getBinaryViaNixDerivation
  , getBinaryViaNixDerivation'

  -- * Introduce a file from a Nix derivation
  , introduceFileViaNixDerivation
  , introduceFileViaNixDerivation'
  , introduceFileViaNixDerivation''
  , getFileViaNixDerivation

  -- * Get a file
  , askFile
  , askFile'

  -- * Helpers for file-finding callbacks
  , defaultFindFile
  , findFirstFile

  -- * Low-level
  , mkFileLabel
  , defaultFileContextVisibilityThreshold

  -- * Types
  , EnvironmentFile(..)
  , HasFile
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.String.Interpolate
import GHC.TypeLits
import Relude
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files.Types
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory


-- | Introduce a file by providing its path.
introduceFile :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  -- | Path to the file
  => FilePath
  -- | Child spec
  -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceFile path = introduceFile' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold }) path

-- | Same as 'introduceFile', but allows passing custom 'NodeOptions'.
introduceFile' :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  => NodeOptions
  -> FilePath
  -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
  -> SpecFree context m ()
introduceFile' nodeOptions path = introduce' nodeOptions [i|#{binaryName} (binary from PATH)|] (mkFileLabel @a) (return $ EnvironmentFile path) (const $ return ())
  where
    -- Saw a bug where we couldn't embed "symbolVal proxy" directly in the quasi-quote above.
    -- Failed with "Couldn't match kind ‘Bool’ with ‘Symbol’"
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

-- | Introduce a file from the PATH, which must be present when tests are run.
-- Useful when you want to set up your own environment with binaries etc. to use in tests.
-- Throws an exception if the desired file is not available.
introduceBinaryViaEnvironment :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  -- | Parent spec
  => SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
  -- | Child spec
  -> SpecFree context m ()
introduceBinaryViaEnvironment = introduceBinaryViaEnvironment' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceBinaryViaEnvironment', but allows you to pass custom 'NodeOptions'.
introduceBinaryViaEnvironment' :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  => NodeOptions
  -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
  -> SpecFree context m ()
introduceBinaryViaEnvironment' nodeOptions = introduce' nodeOptions [i|#{binaryName} (binary from PATH)|] (mkFileLabel @a) alloc cleanup
  where
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

    alloc = do
      liftIO (findExecutable binaryName) >>= \case
        Nothing -> expectationFailure [i|Couldn't find binary '#{binaryName}' on PATH|]
        Just path -> return $ EnvironmentFile path

    cleanup _ = return ()

type NixPackageName = Text

-- | Introduce a given 'EnvironmentFile' from the 'NixContext' in scope.
-- It's recommended to use this with -XTypeApplications.
introduceFileViaNixPackage :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix package name which contains the desired file.
    -- This package will be evaluated using the configured Nixpkgs version of the 'NixContext'.
    NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixPackage name = introduceFileViaNixPackage' @a name (defaultFindFile (symbolVal (Proxy @a)))

-- | Same as 'introduceFileViaNixPackage', but allows you to customize the search callback.
introduceFileViaNixPackage' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix package name which contains the desired file.
    -- This package will be evaluated using the configured Nixpkgs version of the 'NixContext'.
    NixPackageName
    -- | Callback to find the desired file within the Nix derivation path.
    -- It will be passed the derivation path, and should return the file. For example,
    -- tryFindFile "\/nix\/store\/...selenium-server-standalone-3.141.59" may return
    -- "\/nix\/store\/...selenium-server-standalone-3.141.59\/share\/lib\/selenium-server-standalone-3.141.59\/selenium-server-standalone-3.141.59.jar".
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixPackage' = introduceFileViaNixPackage'' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceFileViaNixPackage'', but allows passing custom 'NodeOptions'.
introduceFileViaNixPackage'' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => NodeOptions
    -- | Nix package name which contains the desired file.
    -> NixPackageName
    -- | Callback to find the desired file within the Nix derivation path.
    -- It will be passed the derivation path, and should return the file. For example,
    -- tryFindFile "\/nix\/store\/...selenium-server-standalone-3.141.59" may return
    -- "\/nix\/store\/...selenium-server-standalone-3.141.59\/share\/lib\/selenium-server-standalone-3.141.59\/selenium-server-standalone-3.141.59.jar".
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixPackage'' nodeOptions packageName tryFindFile = introduce' nodeOptions [i|#{binaryName} (file via Nix package #{packageName})|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

    alloc = buildNixSymlinkJoin [packageName] >>= \p -> EnvironmentFile <$> liftIO (tryFindFile p)

-- | Lower-level version of 'introduceFileViaNixPackage'.
getFileViaNixPackage :: forall context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m
  ) =>
    -- | Nix package name which contains the desired file.
    NixPackageName
    -- | Callback to find the desired file, as in 'introduceFileViaNixPackage'.
    -> (FilePath -> IO FilePath)
    -> m FilePath
getFileViaNixPackage packageName tryFindFile = buildNixSymlinkJoin [packageName] >>= liftIO . tryFindFile

-- | Introduce a given 'EnvironmentFile' from the 'NixContext' in scope.
-- It's recommended to use this with -XTypeApplications.
introduceBinaryViaNixPackage :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix package name which contains the desired binary.
    -- This package will be evaluated using the configured Nixpkgs version of the 'NixContext'.
    -- For example, you can use the "hello" binary from the "hello" package like this:
    --
    -- introduceBinaryViaNixPackage' @"hello" "hello"
    NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixPackage = introduceBinaryViaNixPackage' @a (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceBinaryViaNixPackage', but allows passing custom 'NodeOptions'.
introduceBinaryViaNixPackage' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => NodeOptions
    -- | Nix package name which contains the desired binary.
    -> NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixPackage' nodeOptions packageName = introduce' nodeOptions [i|#{binaryName} (binary via Nix package #{packageName})|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

    alloc = buildNixSymlinkJoin [packageName] >>= tryFindBinary binaryName

-- | Lower-level version of 'introduceBinaryViaNixPackage'.
getBinaryViaNixPackage :: forall a context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, KnownSymbol a
  ) =>
    -- | Nix package name which contains the desired binary.
    NixPackageName
    -> m FilePath
getBinaryViaNixPackage packageName = do
  unEnvironmentFile <$> (buildNixSymlinkJoin [packageName] >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Lower-level version of 'introduceBinaryViaNixPackage'.
getBinaryViaNixPackage' :: forall a context m. (
  HasBaseContext context, MonadReader context m
  , MonadLogger m, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | 'NixContext' to use.
    NixContext
    -- | Nix package name which contains the desired binary.
    -> NixPackageName
    -> m FilePath
getBinaryViaNixPackage' nc packageName = do
  unEnvironmentFile <$> (buildNixSymlinkJoin' nc [packageName] >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Introduce a given 'EnvironmentFile' from the 'NixContext' in scope.
-- It's recommended to use this with -XTypeApplications.
introduceBinaryViaNixDerivation :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, MonadMask m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixDerivation = introduceBinaryViaNixDerivation' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceBinaryViaNixDerivation', but allows passing custom 'NodeOptions'.
introduceBinaryViaNixDerivation' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, MonadMask m, KnownSymbol a
  ) => NodeOptions
    -- | Nix derivation as a string.
    -> Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixDerivation' nodeOptions derivation = introduce' nodeOptions [i|#{binaryName} (binary via Nix derivation)|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

    alloc = buildNixCallPackageDerivation derivation >>= tryFindBinary binaryName

-- | Lower-level version of 'introduceBinaryViaNixDerivation'.
getBinaryViaNixDerivation :: forall a context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadMask m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> m FilePath
getBinaryViaNixDerivation derivation =
  unEnvironmentFile <$> (buildNixCallPackageDerivation derivation >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Lower-level version of 'getBinaryViaNixDerivation'.
getBinaryViaNixDerivation' :: forall a context m. (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLoggerIO m, MonadMask m, KnownSymbol a
  )
  -- | Nix context.
  => NixContext
  -- | Nix derivation as a string.
  -> Text
  -> m FilePath
getBinaryViaNixDerivation' nc derivation =
  unEnvironmentFile <$> (buildNixCallPackageDerivation' nc derivation >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Introduce a given 'EnvironmentFile' from the 'NixContext' in scope.
-- It's recommended to use this with -XTypeApplications.
introduceFileViaNixDerivation :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, MonadMask m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation derivation = introduceFileViaNixDerivation' @a derivation (defaultFindFile (symbolVal (Proxy @a)))

-- | Same as 'introduceFileViaNixDerivation', but allows configuring the file finding callback.
introduceFileViaNixDerivation' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, MonadMask m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -- | Callback to find the desired file.
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation' = introduceFileViaNixDerivation'' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceFileViaNixDerivation'', but allows passing custom 'NodeOptions'.
introduceFileViaNixDerivation'' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, MonadMask m, KnownSymbol a
  ) => NodeOptions
    -- | Nix derivation as a string.
    -> Text
    -- | Callback to find the desired file.
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation'' nodeOptions derivation tryFindFile = introduce' nodeOptions [i|#{binaryName} (file via Nix derivation)|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal (Proxy @a)

    alloc = EnvironmentFile <$> (buildNixCallPackageDerivation derivation >>= liftIO . tryFindFile)

-- | Lower-level version of 'introduceFileViaNixDerivation'.
getFileViaNixDerivation :: forall context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadMask m
  ) =>
    -- | Nix derivation as a string.
    Text
    -- | Callback to find the desired file.
    -> (FilePath -> IO FilePath)
    -> m FilePath
getFileViaNixDerivation derivation tryFindFile = buildNixCallPackageDerivation derivation >>= liftIO . tryFindFile


tryFindBinary :: (MonadIO m) => String -> FilePath -> m (EnvironmentFile a)
tryFindBinary binaryName env = do
  findExecutablesInDirectories [env </> "bin"] binaryName >>= \case
    (x:_) -> return $ EnvironmentFile x
    _ -> expectationFailure [i|Couldn't find binary '#{binaryName}' in #{env </> "bin"}|]

-- | Find a file whose name exactly matches a string, using 'findFirstFile'.
-- This calls 'takeFileName', so it only matches against the name, not the relative path.
defaultFindFile :: String -> FilePath -> IO FilePath
defaultFindFile name = findFirstFile (\x -> return (takeFileName x == name))

-- | Find the first file under the given directory (recursively) which matches the predicate.
-- Note that the callback receives the full relative path to the file from the root dir.
-- Throws using 'expectationFailure' when the file is not found.
findFirstFile :: (FilePath -> IO Bool) -> FilePath -> IO FilePath
findFirstFile predicate dir = runExceptT (go dir) >>= \case
  Left x -> return x
  Right () -> expectationFailure [i|Couldn't find file in '#{dir}'|]
  where
    go :: FilePath -> ExceptT FilePath IO ()
    go currentDir = do
      contents <- liftIO $ listDirectory currentDir
      forM_ contents $ \name -> do
        let path = currentDir </> name
        doesDirectoryExist path >>= \case
          True -> go path
          False -> whenM (liftIO $ predicate path) (throwE path)
