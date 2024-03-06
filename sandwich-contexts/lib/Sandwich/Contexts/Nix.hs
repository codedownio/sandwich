{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Nix (
  -- * Nix contexts
  introduceNixContext
  , nixContext
  , NixContext(..)
  , HasNixContext

  -- * Nix environments
  , introduceNixEnvironment
  , buildNixSymlinkJoin
  , buildNixCallPackageDerivation
  , buildNixExpression
  , nixEnvironment
  , HasNixEnvironment

  -- * Nixpkgs releases
  , nixpkgsRelease2311
  , nixpkgsReleaseDefault
  -- TODO: export smart constructors for this
  , NixpkgsDerivation(..)
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Relude
import Sandwich.Contexts.Util.Aeson
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.MVar (modifyMVar)
import UnliftIO.Process

-- * Types

nixContext :: Label "nixContext" NixContext
nixContext = Label

data NixContext = NixContext {
  nixContextNixBinary :: FilePath
  , nixContextNixpkgsDerivation :: NixpkgsDerivation
  , nixContextBuildCache :: MVar (Map Text (Async FilePath))
  }

type HasNixContext context = HasLabel context "nixContext" NixContext

nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label

type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath


data NixpkgsDerivation =
  NixpkgsDerivationFetchFromGitHub {
    nixpkgsDerivationOwner :: Text
    , nixpkgsDerivationRepo :: Text
    , nixpkgsDerivationRev :: Text
    , nixpkgsDerivationSha256 :: Text
    } deriving (Show, Eq)

-- | Nixpkgs release 23.11, accessed 2/19/2023.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-23.11
nixpkgsRelease2311 :: NixpkgsDerivation
nixpkgsRelease2311 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "cc86e0769882886f7831de9c9373b62ea2c06e3f"
  , nixpkgsDerivationSha256 = "sha256-1eAZINWjTTA8nWJiN979JVSwvCYzUWnMpzMHGUCLgZk="
  }

nixpkgsReleaseDefault :: NixpkgsDerivation
nixpkgsReleaseDefault = nixpkgsRelease2311

introduceNixContext :: (
  MonadUnliftIO m, MonadThrow m
  ) => NixpkgsDerivation -> SpecFree (LabelValue "nixContext" NixContext :> context) m () -> SpecFree context m ()
introduceNixContext nixpkgsDerivation = introduce "Introduce Nix context" nixContext getNixContext (const $ return ())
  where
    getNixContext = findExecutable "nix" >>= \case
      Nothing -> expectationFailure [i|Couldn't find "nix" binary when introducing Nix context. A Nix binary and store must already be available in the environment.|]
      Just p -> do
        -- TODO: make sure the Nixpkgs derivation works
        buildCache <- newMVar mempty
        pure (NixContext p nixpkgsDerivation buildCache)

introduceNixEnvironment :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m
  ) => [Text] -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment packageNames = introduce "Introduce Nix environment" nixEnvironment (buildNixSymlinkJoin packageNames) (const $ return ())

-- | Build a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix "symlinkJoin" function. Their binaries will generally
-- be found in "<environment path>/bin".
buildNixSymlinkJoin :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => [Text] -> m FilePath
buildNixSymlinkJoin packageNames = do
  NixContext {..} <- getContext nixContext
  buildNixExpression $ renderNixSymlinkJoin nixContextNixpkgsDerivation packageNames

-- | Build a Nix environment expressed as a derivation expecting a list of dependencies, as in the
-- Nix "callPackage" design pattern. I.e.
-- "{ git, gcc, stdenv, ... }: stdenv.mkDerivation {...}"
buildNixCallPackageDerivation :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => Text -> m FilePath
buildNixCallPackageDerivation derivation = do
  NixContext {..} <- getContext nixContext

  wait =<< modifyMVar nixContextBuildCache (\m ->
    case M.lookup derivation m of
      Just x -> return (m, x)
      Nothing -> do
        asy <- async $ do
          Just dir <- getCurrentFolder
          gcrootDir <- liftIO $ createTempDirectory dir "nix-expression"
          let derivationPath = gcrootDir </> "default.nix"
          liftIO $ T.writeFile derivationPath derivation
          runNixBuild (renderCallPackageDerivation nixContextNixpkgsDerivation derivationPath) (gcrootDir </> "gcroot")

        return (M.insert derivation asy m, asy)
    )


-- | Build a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix "symlinkJoin" function. Their binaries will generally
-- be found in "<environment path>/bin".
buildNixExpression :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => Text -> m FilePath
buildNixExpression expr = do
  NixContext {..} <- getContext nixContext

  wait =<< modifyMVar nixContextBuildCache (\m ->
    case M.lookup expr m of
      Just x -> return (m, x)
      Nothing -> do
        asy <- async $ do
          Just dir <- getCurrentFolder
          gcrootDir <- liftIO $ createTempDirectory dir "nix-expression"
          runNixBuild expr (gcrootDir </> "gcroot")

        return (M.insert expr asy m, asy)
    )


runNixBuild :: (MonadUnliftIO m, MonadLogger m) => Text -> String -> m String
runNixBuild expr outputPath = do
  output <- readCreateProcessWithLogging (
    proc "nix" ["build"
               , "--impure"
               , "--expr", toString expr
               , "-o", outputPath
               , "--json"
               ]
    ) ""

  case A.eitherDecodeStrict (encodeUtf8 output) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure (toString p)
    x -> expectationFailure [i|Couldn't parse Nix build JSON output: #{x} (output was #{output})|]


renderNixSymlinkJoin :: NixpkgsDerivation -> [Text] -> Text
renderNixSymlinkJoin (NixpkgsDerivationFetchFromGitHub {..}) packageNames = [i|
\# Use the ambient <nixpkgs> channel to bootstrap
with {
  inherit (import (<nixpkgs>) {})
  fetchgit fetchFromGitHub;
};

let
  nixpkgs = fetchFromGitHub {
    owner = "#{nixpkgsDerivationOwner}";
    repo = "#{nixpkgsDerivationRepo}";
    rev = "#{nixpkgsDerivationRev}";
    sha256 = "#{nixpkgsDerivationSha256}";
  };

  pkgs = import nixpkgs {};
in

pkgs.symlinkJoin { name = "test-contexts-environment"; paths = with pkgs; [#{T.intercalate " " packageNames}]; }
|]

renderCallPackageDerivation :: NixpkgsDerivation -> FilePath -> Text
renderCallPackageDerivation (NixpkgsDerivationFetchFromGitHub {..}) derivationPath = [i|
\# Use the ambient <nixpkgs> channel to bootstrap
with {
  inherit (import (<nixpkgs>) {})
  fetchgit fetchFromGitHub;
};

let
  nixpkgs = fetchFromGitHub {
    owner = "#{nixpkgsDerivationOwner}";
    repo = "#{nixpkgsDerivationRepo}";
    rev = "#{nixpkgsDerivationRev}";
    sha256 = "#{nixpkgsDerivationSha256}";
  };

  pkgs = import nixpkgs {};
in

pkgs.callPackage #{show derivationPath :: String} {}
|]
