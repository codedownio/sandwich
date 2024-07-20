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

  -- * Types
  , EnvironmentFile(..)
  , HasFile
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.String.Interpolate
import GHC.TypeLits
import Relude
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory


-- | A file path to make available to tests.
-- For example, this can be an external binary like "minikube" if a given test context wants
-- to use it to start a Minikube cluster.
-- But you can use this for any kind of file you want to inject into tests.
data EnvironmentFile a = EnvironmentFile { unEnvironmentFile :: FilePath }

-- | Has-* class for asserting a given file is available.
type HasFile context a = HasLabel context (AppendSymbol "file-" a) (EnvironmentFile a)

mkFileLabel :: Label (AppendSymbol "file-" a) (EnvironmentFile a)
mkFileLabel = Label

-- | Retrieve a file context.
askFile :: forall a context m. (MonadReader context m, HasFile context a) => m FilePath
askFile = askFile' (Proxy @a)

-- | Variant of 'askFile' that you can use with a 'Proxy' rather than a type application.
askFile' :: forall a context m. (MonadReader context m, HasFile context a) => Proxy a -> m FilePath
askFile' _ = unEnvironmentFile <$> getContext (mkFileLabel @a)

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
introduceFile path = introduceFile' (Proxy @a) path

introduceFile' :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  -- | Proxy for the file type to use. I.e. 'Proxy "my-file"'
  => Proxy a -> FilePath -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m () -> SpecFree context m ()
introduceFile' proxy path = introduce [i|#{binaryName} (binary from PATH)|] (mkFileLabel @a) (return $ EnvironmentFile path) (const $ return ())
  where
    -- Saw a bug where we couldn't embed "symbolVal proxy" directly in the quasi-quote above.
    -- Failed with "Couldn't match kind ‘Bool’ with ‘Symbol’"
    binaryName :: String
    binaryName = symbolVal proxy

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
introduceBinaryViaEnvironment = introduceBinaryViaEnvironment' (Proxy @a)

-- | Variant of 'introduceBinaryViaEnvironment' that you can use with a 'Proxy' rather
-- than a type application.
introduceBinaryViaEnvironment' :: forall a context m. (
  MonadUnliftIO m, KnownSymbol a
  )
  -- | Proxy for the file type to use. I.e. 'Proxy "my-file"'
  => Proxy a
  -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
  -> SpecFree context m ()
introduceBinaryViaEnvironment' proxy = introduce [i|#{binaryName} (binary from PATH)|] (mkFileLabel @a) alloc cleanup
  where
    binaryName :: String
    binaryName = symbolVal proxy

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
introduceFileViaNixPackage' = introduceFileViaNixPackage'' (Proxy @a)

-- | Same as 'introduceFileViaNixPackage'', but allows passing a 'Proxy'.
introduceFileViaNixPackage'' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix package name which contains the desired file.
    -> NixPackageName
    -- | Callback to find the desired file within the Nix derivation path.
    -- It will be passed the derivation path, and should return the file. For example,
    -- tryFindFile "\/nix\/store\/...selenium-server-standalone-3.141.59" may return
    -- "\/nix\/store\/...selenium-server-standalone-3.141.59\/share\/lib\/selenium-server-standalone-3.141.59\/selenium-server-standalone-3.141.59.jar".
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixPackage'' proxy packageName tryFindFile = introduce [i|#{binaryName} (file via Nix package #{packageName})|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal proxy

    alloc = buildNixSymlinkJoin [packageName] >>= \p -> EnvironmentFile <$> liftIO (tryFindFile p)

-- | Lower-level version of 'introduceFileViaNixPackage'.
getFileViaNixPackage :: forall context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m
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
introduceBinaryViaNixPackage = introduceBinaryViaNixPackage' (Proxy @a)

-- | Same as 'introduceBinaryViaNixPackage', but allows passing a 'Proxy'.
introduceBinaryViaNixPackage' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix package name which contains the desired binary.
    -> NixPackageName
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixPackage' proxy packageName = introduce [i|#{binaryName} (binary via Nix package #{packageName})|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal proxy

    alloc = buildNixSymlinkJoin [packageName] >>= tryFindBinary binaryName

-- | Lower-level version of 'introduceBinaryViaNixPackage'.
getBinaryViaNixPackage :: forall a context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m, KnownSymbol a
  ) =>
    -- | Nix package name which contains the desired binary.
    NixPackageName
    -> m FilePath
getBinaryViaNixPackage packageName = do
  unEnvironmentFile <$> (buildNixSymlinkJoin [packageName] >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Lower-level version of 'introduceBinaryViaNixPackage'.
getBinaryViaNixPackage' :: forall a context m. (
  HasBaseContext context, MonadReader context m
  , MonadLogger m, MonadUnliftIO m, MonadFail m, KnownSymbol a
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
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixDerivation = introduceBinaryViaNixDerivation' (Proxy @a)

-- | Same as 'introduceBinaryViaNixDerivation', but allows passing a 'Proxy'.
introduceBinaryViaNixDerivation' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix derivation as a string.
    -> Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceBinaryViaNixDerivation' proxy derivation = introduce [i|#{binaryName} (binary via Nix derivation)|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal proxy

    alloc = buildNixCallPackageDerivation derivation >>= tryFindBinary binaryName

-- | Lower-level version of 'introduceBinaryViaNixDerivation'.
getBinaryViaNixDerivation :: forall a context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> m FilePath
getBinaryViaNixDerivation derivation =
  unEnvironmentFile <$> (buildNixCallPackageDerivation derivation >>= tryFindBinary (symbolVal (Proxy @a)))

-- | Lower-level version of 'getBinaryViaNixDerivation'.
getBinaryViaNixDerivation' :: forall a context m. (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m, KnownSymbol a
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
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation derivation = introduceFileViaNixDerivation' @a derivation (defaultFindFile (symbolVal (Proxy @a)))

-- | Same as 'introduceFileViaNixDerivation', but allows configuring the file finding callback.
introduceFileViaNixDerivation' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) =>
    -- | Nix derivation as a string.
    Text
    -- | Callback to find the desired file.
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation' = introduceFileViaNixDerivation'' (Proxy @a)

-- | Same as 'introduceFileViaNixDerivation'', but allows passing a 'Proxy'.
introduceFileViaNixDerivation'' :: forall a context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m, KnownSymbol a
  ) => Proxy a
    -- | Nix derivation as a string.
    -> Text
    -- | Callback to find the desired file.
    -> (FilePath -> IO FilePath)
    -> SpecFree (LabelValue (AppendSymbol "file-" a) (EnvironmentFile a) :> context) m ()
    -> SpecFree context m ()
introduceFileViaNixDerivation'' proxy derivation tryFindFile = introduce [i|#{binaryName} (file via Nix derivation)|] (mkFileLabel @a) alloc (const $ return ())
  where
    binaryName :: String
    binaryName = symbolVal proxy

    alloc = EnvironmentFile <$> (buildNixCallPackageDerivation derivation >>= liftIO . tryFindFile)

-- | Lower-level version of 'introduceFileViaNixDerivation'.
getFileViaNixDerivation :: forall context m. (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadFail m
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
