{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module contains tools for working with Nix, in order to provide Nix-built artifacts to tests.

The Nix package set (Nixpkgs) is one of the largest package sets in the world, and can be a great way to get artifacts reproducibly. All you need is a @nix@ binary available on the PATH.

For example, the following will build a Nix environment based on Nixpkgs release 24.05, containing Emacs and Firefox.

@
introduceNixContext nixpkgsRelease2405 $
  introduceNixEnvironment ["emacs", "firefox"] $ do
    it "uses the environment" $ do
      envPath <- getContext nixEnvironment

      emacsVersion <- readCreateProcess (proc (envPath <\/\> "bin" <\/\> "emacs") ["--version"]) ""
      info [i|Emacs version: #{emacsVersion}|]

      firefoxVersion <- readCreateProcess (proc (envPath <\/\> "bin" <\/\> "firefox") ["--version"]) ""
      info [i|Firefox version: #{firefoxVersion}|]
@

-}

module Test.Sandwich.Contexts.Nix (
  -- * Nix contexts
  introduceNixContext
  , introduceNixContext'
  , introduceNixContext''
  , makeNixContext
  , makeNixContext'

  -- * Nix environments
  , introduceNixEnvironment
  , introduceNixEnvironment'
  , buildNixPackage
  , buildNixPackage'
  , buildNixSymlinkJoin
  , buildNixSymlinkJoin'
  , buildNixExpression
  , buildNixExpression'
  , buildNixCallPackageDerivation
  , buildNixCallPackageDerivation'

  -- * Nixpkgs releases #releases#
  , nixpkgsReleaseDefault
  , nixpkgsMaster
  , nixpkgsRelease2505
  , nixpkgsRelease2411
  , nixpkgsRelease2405
  , nixpkgsRelease2311

  -- * Types
  , nixContext
  , NixContext(..)
  , HasNixContext

  , nixEnvironment
  , HasNixEnvironment

  , NixpkgsDerivation(..)

  , defaultFileContextVisibilityThreshold
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
import System.IO.Temp (createTempDirectory)
import Test.Sandwich
import Test.Sandwich.Contexts.Files.Types
import Test.Sandwich.Contexts.Util.Aeson
import qualified Text.Show
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.MVar (modifyMVar)
import UnliftIO.Process
import UnliftIO.Temporary

-- * Types

nixContext :: Label "nixContext" NixContext
nixContext = Label

data NixContext = NixContext {
  nixContextNixBinary :: FilePath
  , nixContextNixpkgsDerivation :: NixpkgsDerivation
  , nixContextBuildCache :: MVar (Map Text (Async FilePath))
  }
instance Show NixContext where
  show (NixContext {..}) = [i|NixContext<#{nixContextNixBinary}, #{nixContextNixpkgsDerivation}>|]

type HasNixContext context = HasLabel context "nixContext" NixContext

nixEnvironment :: Label "nixEnvironment" FilePath
nixEnvironment = Label

type HasNixEnvironment context = HasLabel context "nixEnvironment" FilePath

defaultFileContextVisibilityThreshold :: Int
defaultFileContextVisibilityThreshold = 150

data NixpkgsDerivation =
  NixpkgsDerivationFetchFromGitHub {
    nixpkgsDerivationOwner :: Text
    , nixpkgsDerivationRepo :: Text
    , nixpkgsDerivationRev :: Text
    , nixpkgsDerivationSha256 :: Text

    -- | Set the environment variable NIXPKGS_ALLOW_UNFREE=1 when building with this derivation.
    -- Useful when you want to use packages with unfree licenses, like @google-chrome@.
    , nixpkgsDerivationAllowUnfree :: Bool
    } deriving (Show, Eq)

-- | Nixpkgs master, accessed 6\/28\/2025.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev master
nixpkgsMaster :: NixpkgsDerivation
nixpkgsMaster = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "40a8fd31ccc6d87eff8e8aae833d213b10bb9f50"
  , nixpkgsDerivationSha256 = "sha256-HXlk/16dnoQqy2wjizF1uA7b1gNS8iYFq7vSAG2P8dk="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Nixpkgs release 25.05, accessed 6\/6\/2025.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-25.05
nixpkgsRelease2505 :: NixpkgsDerivation
nixpkgsRelease2505 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "8217c6edf391991f07ecacf3d31ba6eb01d733b1"
  , nixpkgsDerivationSha256 = "sha256-aaeXPG9zVvi+aKTp0dMUYOeMuhDXQejRPh2CfK23nf8="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Nixpkgs release 24.11, accessed 6\/6\/2025.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-24.11
nixpkgsRelease2411 :: NixpkgsDerivation
nixpkgsRelease2411 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "5908ad2494520214a309e74d5c3f33623a593ecd"
  , nixpkgsDerivationSha256 = "sha256-0q80SLtfhrtZAzLGpwAQjqaTE+HAwmOjoX4Q3M5mB/s="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Nixpkgs release 24.05, accessed 11\/9\/2024.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-24.05
nixpkgsRelease2405 :: NixpkgsDerivation
nixpkgsRelease2405 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "bb824c634c812feede9d398c000526401028c0e7"
  , nixpkgsDerivationSha256 = "sha256-xYnWv9kyJyF8rEZ1uJaSek2fmaIowkk/ovE6+MwcP2E="
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
--
-- The 'NixContext' contains a build cache, so if you build a given derivation more than
-- once in your tests under this node, runs after the first one will be fast.
--
-- This function requires a @nix@ binary to be in the PATH and will throw an exception if it
-- isn't found.
introduceNixContext :: (
  MonadUnliftIO m, MonadThrow m
  )
  -- | Nixpkgs derivation to use
  => NixpkgsDerivation
  -- | Child spec
  -> SpecFree (LabelValue "nixContext" NixContext :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceNixContext = introduceNixContext' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

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
introduceNixContext' nodeOptions nixpkgsDerivation = introduce' nodeOptions "Introduce Nix context" nixContext (makeNixContext nixpkgsDerivation) (const $ return ())

-- | Same as 'introduceNixContext'', but allows specifying the Nix binary via 'HasFile'.
introduceNixContext'' :: (
  MonadUnliftIO m
  , MonadThrow m
  , HasFile context "nix"
  )
  -- | Custom 'NodeOptions'
  => NodeOptions
  -- | Nixpkgs derivation to use
  -> NixpkgsDerivation
  -- | Child spec
  -> SpecFree (LabelValue "nixContext" NixContext :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceNixContext'' nodeOptions nixpkgsDerivation = introduce' nodeOptions "Introduce Nix context" nixContext (makeNixContext' nixpkgsDerivation) (const $ return ())

-- | Build a 'NixContext' directly. Throws an exception if the @nix@ binary is not found.
makeNixContext :: (MonadIO m) => NixpkgsDerivation -> m NixContext
makeNixContext nixpkgsDerivation = findExecutable "nix" >>= \case
  Nothing -> expectationFailure [i|Couldn't find "nix" binary when introducing Nix context. A Nix binary and store must already be available in the environment.|]
  Just p -> do
    -- TODO: make sure the Nixpkgs derivation works
    buildCache <- newMVar mempty
    pure (NixContext p nixpkgsDerivation buildCache)

-- | Build a 'NixContext' directly, specifying the Nix binary via 'HasFile'.
makeNixContext' :: (MonadIO m, MonadReader ctx m, HasFile ctx "nix") => NixpkgsDerivation -> m NixContext
makeNixContext' nixpkgsDerivation = do
  nix <- askFile @"nix"
  buildCache <- newMVar mempty
  pure (NixContext nix nixpkgsDerivation buildCache)

-- | Introduce a Nix environment containing the given list of packages, using the current 'NixContext'.
-- These packages are mashed together using the Nix @symlinkJoin@ function. Their binaries will generally
-- be found in @\<environment path\>\/bin@.
introduceNixEnvironment :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m
  )
  -- | List of package names to include in the Nix environment
  => [Text]
  -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m ()
  -> SpecFree context m ()
introduceNixEnvironment = introduceNixEnvironment' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceNixEnvironment', but allows passing custom 'NodeOptions'.
introduceNixEnvironment' :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m
  )
  -- | Custom 'NodeOptions'
  => NodeOptions
  -- | List of package names to include in the Nix environment
  -> [Text]
  -> SpecFree (LabelValue "nixEnvironment" FilePath :> context) m ()
  -> SpecFree context m ()
introduceNixEnvironment' nodeOptions packageNames = introduce' nodeOptions "Introduce Nix environment" nixEnvironment (buildNixSymlinkJoin packageNames) (const $ return ())

-- | Build a single Nix package name from Nixpkgs
buildNixPackage :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Package name.
  => Text
  -> m FilePath
buildNixPackage packageName = do
  nc <- getContext nixContext
  buildNixPackage' nc packageName

-- | Lower-level version of 'buildNixCallPackageDerivation'
buildNixPackage' :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Nix context.
  => NixContext
  -- | Package name.
  -> Text
  -> m FilePath
buildNixPackage' nc packageName = buildNixCallPackageDerivation' nc expr
  where
    expr = [i|{ pkgs }: pkgs."#{packageName}"|]

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
  , MonadUnliftIO m, MonadLogger m
  )
  -- | Nix derivation
  => Text
  -> m FilePath
buildNixCallPackageDerivation derivation = do
  nc <- getContext nixContext
  buildNixCallPackageDerivation' nc derivation

-- | Lower-level version of 'buildNixCallPackageDerivation'
buildNixCallPackageDerivation' :: forall context m. (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadLogger m
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
    withDerivationPath :: Maybe FilePath -> (FilePath -> m a) -> m a
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
                 ] <> (case maybeOutputPath of Nothing -> ["--no-link"]; Just p -> ["-o", p])
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

renderDerivationWithPkgs :: NixpkgsDerivation -> Text -> Text
renderDerivationWithPkgs (NixpkgsDerivationFetchFromGitHub {..}) expr = [i|
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

#{expr}
|]

renderCallPackageDerivation :: NixpkgsDerivation -> FilePath -> Text
renderCallPackageDerivation nixpkgsDerivation derivationPath =
  renderDerivationWithPkgs nixpkgsDerivation expr
  where
    expr = [i|pkgs.callPackage #{show derivationPath :: String} {}|]
