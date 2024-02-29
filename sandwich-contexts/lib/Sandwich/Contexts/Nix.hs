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
  , buildNixEnvironment
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
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude
import Sandwich.Contexts.Util.Aeson
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import UnliftIO.Directory
import UnliftIO.Process

-- * Types

nixContext :: Label "nixContext" NixContext
nixContext = Label

data NixContext = NixContext {
  nixContextNixBinary :: FilePath
  , nixContextNixpkgsDerivations :: NixpkgsDerivation
  } deriving (Show, Eq)

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
        pure (NixContext p nixpkgsDerivation)

introduceNixEnvironment :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m
  ) => [Text] -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m () -> SpecFree context m ()
introduceNixEnvironment packageNames = introduce "Introduce Nix environment" nixEnvironment (buildNixEnvironment packageNames) (const $ return ())

-- | Build a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix "symlinkJoin" function. Their binaries will generally
-- be found in "<environment path>/bin".
buildNixEnvironment :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => [Text] -> m FilePath
buildNixEnvironment packageNames = do
  Just dir <- getCurrentFolder
  gcrootDir <- liftIO $ createTempDirectory dir "nix-environment"

  NixContext {..} <- getContext nixContext
  output <- readCreateProcessWithLogging (
    proc "nix" ["build"
               , "--impure"
               , "--expr", renderNixEnvironment nixContextNixpkgsDerivations packageNames
               , "-o", gcrootDir </> "gcroot"
               , "--json"
               ]
    ) ""

  case A.eitherDecodeStrict (encodeUtf8 output) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure (toString p)
    x -> expectationFailure [i|Couldn't parse Nix build JSON output: #{x} (output was #{output})|]


renderNixEnvironment :: NixpkgsDerivation -> [Text] -> String
renderNixEnvironment (NixpkgsDerivationFetchFromGitHub {..}) packageNames = [i|
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
