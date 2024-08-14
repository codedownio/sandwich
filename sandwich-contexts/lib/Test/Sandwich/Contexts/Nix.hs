{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Nix (
  -- * Nix contexts
  introduceNixContext
  , introduceNixContext'

  -- * Nix environments
  , introduceNixEnvironment
  , introduceNixEnvironment'
  , buildNixSymlinkJoin
  , buildNixSymlinkJoin'
  , buildNixExpression
  , buildNixExpression'
  , buildNixCallPackageDerivation
  , buildNixCallPackageDerivation'

  -- * Nixpkgs releases
  , nixpkgsReleaseDefault
  , nixpkgsRelease2405
  , nixpkgsRelease2311

  -- * Types
  , nixContext
  , NixContext(..)
  , HasNixContext

  , nixEnvironment
  , HasNixEnvironment

  , NixpkgsDerivation(..)
  ) where

import Control.Monad.Catch (MonadMask, MonadThrow)
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
import qualified Text.Show
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
instance Show NixContext where
  show (NixContext {}) = "<NixContext>"

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

-- | Currently set to 'nixpkgsRelease2405'.
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
introduceNixContext = introduceNixContext' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Same as 'introduceNixContext', but allows passing custom 'NodeOptions'.
introduceNixContext' :: (
  MonadUnliftIO m, MonadThrow m
  )
  -- | Custom 'NodeOptions'
  => NodeOptions
  -- | Nixpkgs derivation to use
  -> NixpkgsDerivation
  -- | Child spec
  -> SpecFree (LabelValue "nixContext" NixContext :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceNixContext' nodeOptions nixpkgsDerivation = introduce' nodeOptions "Introduce Nix context" nixContext getNixContext (const $ return ())
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
introduceNixEnvironment = introduceNixEnvironment' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Same as 'introduceNixEnvironment', but allows passing custom 'NodeOptions'.
introduceNixEnvironment' :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m
  )
  -- | Custom 'NodeOptions'
  => NodeOptions
  -- | List of package names to include in the Nix environment
  -> [Text]
  -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m ()
  -> SpecFree context m ()
introduceNixEnvironment' nodeOptions packageNames = introduce' nodeOptions "Introduce Nix environment" nixEnvironment (buildNixSymlinkJoin packageNames) (const $ return ())

-- | Build a Nix environment, as in 'introduceNixEnvironment'.
buildNixSymlinkJoin :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Package names
  => [Text] -> m FilePath
buildNixSymlinkJoin packageNames = do
  NixContext {..} <- getContext nixContext
  buildNixExpression $ renderNixSymlinkJoin nixContextNixpkgsDerivation packageNames

-- | Lower-level version of 'buildNixSymlinkJoin'.
buildNixSymlinkJoin' :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Nix context
  => NixContext
  -- | Package names
  -> [Text]
  -> m FilePath
buildNixSymlinkJoin' nc@(NixContext {..}) packageNames = do
  buildNixExpression' nc $ renderNixSymlinkJoin nixContextNixpkgsDerivation packageNames

-- | Build a Nix environment expressed as a derivation expecting a list of dependencies, as in the
-- Nix "callPackage" design pattern. I.e.
-- "{ git, gcc, stdenv, ... }: stdenv.mkDerivation {...}"
buildNixCallPackageDerivation :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m, MonadMask m
  )
  -- | Nix derivation
  => Text
  -> m FilePath
buildNixCallPackageDerivation derivation = do
  nc <- getContext nixContext
  buildNixCallPackageDerivation' nc derivation

-- | Lower-level version of 'buildNixCallPackageDerivation'
buildNixCallPackageDerivation' :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLogger m, MonadMask m
  )
  -- | Nix context.
  => NixContext
  -- | Nix derivation.
  -> Text
  -> m FilePath
buildNixCallPackageDerivation' nc@(NixContext {..}) derivation = do
  wait =<< modifyMVar nixContextBuildCache (\m ->
    case M.lookup derivation m of
      Just x -> return (m, x)
      Nothing -> do
        asy <- async $ do
          maybeNixExpressionDir <- getCurrentFolder >>= \case
            Just dir -> (Just <$>) $ liftIO $ createTempDirectory dir "nix-expression"
            Nothing -> return Nothing

          withDerivationPath maybeNixExpressionDir $ \derivationPath -> do
            liftIO $ T.writeFile derivationPath derivation
            runNixBuild' nc (renderCallPackageDerivation nixContextNixpkgsDerivation derivationPath) ((</> "gcroot") <$> maybeNixExpressionDir)

        return (M.insert derivation asy m, asy)
    )
  where
    withDerivationPath (Just nixExpressionDir) action = action (nixExpressionDir </> "default.nix")
    withDerivationPath Nothing action = withSystemTempDirectory "nix-expression" $ \dir -> action (dir </> "default.nix")


-- | Build a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix "symlinkJoin" function. Their binaries will generally
-- be found in "\<environment path\>\/bin".
buildNixExpression :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Nix expression
  => Text -> m FilePath
buildNixExpression expr = getContext nixContext >>= (`buildNixExpression'` expr)

-- | Lower-level version of 'buildNixExpression'.
buildNixExpression' :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Nix expression
  => NixContext -> Text -> m FilePath
buildNixExpression' nc@(NixContext {..}) expr = do
  wait =<< modifyMVar nixContextBuildCache (\m ->
    case M.lookup expr m of
      Just x -> return (m, x)
      Nothing -> do
        asy <- async $ do
          maybeNixExpressionDir <- getCurrentFolder >>= \case
            Just dir -> (Just <$>) $ liftIO $ createTempDirectory dir "nix-expression"
            Nothing -> pure Nothing
          runNixBuild' nc expr ((</> "gcroot") <$> maybeNixExpressionDir)

        return (M.insert expr asy m, asy)
    )

-- runNixBuild :: (MonadUnliftIO m, MonadLogger m, MonadReader context m, HasNixContext context) => Text -> String -> m String
-- runNixBuild expr outputPath = do
--   nc <- getContext nixContext
--   runNixBuild' nc expr outputPath

runNixBuild' :: (MonadUnliftIO m, MonadLogger m) => NixContext -> Text -> Maybe String -> m String
runNixBuild' (NixContext {nixContextNixpkgsDerivation}) expr maybeOutputPath = do
  maybeEnv <- case nixpkgsDerivationAllowUnfree nixContextNixpkgsDerivation of
    False -> pure Nothing
    True -> do
      env <- getEnvironment
      return $ Just (("NIXPKGS_ALLOW_UNFREE", "1") : env)

  -- TODO: switch this to using nix-build so we can avoid the "--impure" flag?
  output <- readCreateProcessWithLogging (
    (proc "nix" (["build"
                 , "--impure"
                 , "--extra-experimental-features", "nix-command"
                 , "--expr", toString expr
                 , "--json"
                 ] <> (case maybeOutputPath of Nothing -> []; Just p -> ["-o", p])
                )) { env = maybeEnv }
    ) ""

  case A.eitherDecodeStrict (encodeUtf8 output) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure (toString p)
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "bin" -> Just (A.String p))))):_))) -> pure (toString p)
    x -> expectationFailure [i|Couldn't parse Nix build JSON output: #{x} (output was #{A.encode output})|]

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
