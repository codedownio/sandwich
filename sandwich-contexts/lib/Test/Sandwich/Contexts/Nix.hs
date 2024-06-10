{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Nix (
  -- * Nix contexts
  introduceNixContext

  -- * Nix environments
  , introduceNixEnvironment
  , buildNixSymlinkJoin
  , buildNixExpression
  , buildNixCallPackageDerivation

  -- * Nixpkgs releases
  , nixpkgsRelease2311
  , nixpkgsReleaseDefault
  -- TODO: export smart constructors for this

  -- * Types
  , nixContext
  , NixContext(..)
  , HasNixContext

  , nixEnvironment
  , HasNixEnvironment

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
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Util.Aeson
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Environment
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

    -- | Set the environment variable NIXPKGS_ALLOW_UNFREE=1 when building with this derivation.
    -- Useful when you want to use packages with unfree licenses, like "google-chrome".
    , nixpkgsDerivationAllowUnfree :: Bool
    } deriving (Show, Eq)

-- | Nixpkgs release 24.05, accessed 6\/10\/2024.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-24.05
nixpkgsRelease2405 :: NixpkgsDerivation
nixpkgsRelease2405 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "869cab745a802b693b45d193b460c9184da671f3"
  , nixpkgsDerivationSha256 = "sha256-zliqz7ovpxYdKIK+GlWJZxifXsT9A1CHNQhLxV0G1Hc="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Nixpkgs release 23.11, accessed 2\/19\/2023.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-23.11
nixpkgsRelease2311 :: NixpkgsDerivation
nixpkgsRelease2311 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "cc86e0769882886f7831de9c9373b62ea2c06e3f"
  , nixpkgsDerivationSha256 = "sha256-1eAZINWjTTA8nWJiN979JVSwvCYzUWnMpzMHGUCLgZk="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Currently set to 'nixpkgsRelease2311'.
nixpkgsReleaseDefault :: NixpkgsDerivation
nixpkgsReleaseDefault = nixpkgsRelease2405

-- | Introduce a 'NixContext', which contains information about where to find Nix and what
-- version of Nixpkgs to use. This can be leveraged to introduce Nix packages in tests.
introduceNixContext :: (
  MonadUnliftIO m, MonadThrow m
  )
  -- | Nixpkgs derivation to use
  => NixpkgsDerivation
  -- | Child spec
  -> SpecFree (LabelValue "nixContext" NixContext :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceNixContext nixpkgsDerivation = introduce "Introduce Nix context" nixContext getNixContext (const $ return ())
  where
    getNixContext = findExecutable "nix" >>= \case
      Nothing -> expectationFailure [i|Couldn't find "nix" binary when introducing Nix context. A Nix binary and store must already be available in the environment.|]
      Just p -> do
        -- TODO: make sure the Nixpkgs derivation works
        buildCache <- newMVar mempty
        pure (NixContext p nixpkgsDerivation buildCache)

-- | Introduce a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix "symlinkJoin" function. Their binaries will generally
-- be found in "\<environment path\>\/bin".
introduceNixEnvironment :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m
  )
  -- | List of package names to include in the Nix environment
  => [Text]
  -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m ()
  -> SpecFree context m ()
introduceNixEnvironment packageNames = introduce "Introduce Nix environment" nixEnvironment (buildNixSymlinkJoin packageNames) (const $ return ())

-- | Build a Nix environment, as in 'introduceNixEnvironment'.
buildNixSymlinkJoin :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  )
  -- | Package names
  => [Text] -> m FilePath
buildNixSymlinkJoin packageNames = do
  NixContext {..} <- getContext nixContext
  buildNixExpression $ renderNixSymlinkJoin nixContextNixpkgsDerivation packageNames

-- | Build a Nix environment expressed as a derivation expecting a list of dependencies, as in the
-- Nix "callPackage" design pattern. I.e.
-- "{ git, gcc, stdenv, ... }: stdenv.mkDerivation {...}"
buildNixCallPackageDerivation :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  )
  -- | Nix derivation
  => Text
  -> m FilePath
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
-- be found in "\<environment path\>\/bin".
buildNixExpression :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  )
  -- | Nix expression
  => Text -> m FilePath
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


runNixBuild :: (MonadUnliftIO m, MonadLogger m, MonadReader context m, HasNixContext context) => Text -> String -> m String
runNixBuild expr outputPath = do
  NixContext {nixContextNixpkgsDerivation} <- getContext nixContext
  maybeEnv <- case nixpkgsDerivationAllowUnfree nixContextNixpkgsDerivation of
    False -> pure Nothing
    True -> do
      env <- getEnvironment
      return $ Just (("NIXPKGS_ALLOW_UNFREE", "1") : env)

  output <- readCreateProcessWithLogging (
    (proc "nix" ["build"
               , "--impure"
               , "--expr", toString expr
               , "-o", outputPath
               , "--json"
               ]) { env = maybeEnv }
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
