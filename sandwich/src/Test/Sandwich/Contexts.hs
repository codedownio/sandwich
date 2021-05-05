{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for retrieving context information from within tests.

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import GHC.Stack
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


-- | Get a context by its label.
getContext :: (Monad m, HasLabel context l a, HasCallStack, MonadReader context m) => Label l a -> m a
getContext = asks . getLabelValue

-- | Get the root folder of the on-disk test tree for the current run.
-- Will be 'Nothing' if the run isn't configured to use the disk.
getRunRoot :: (Monad m, HasBaseContext context, MonadReader context m) => m (Maybe FilePath)
getRunRoot = asks (baseContextRunRoot . getBaseContext)

-- | Get the on-disk folder corresponding to the current node.
-- Will be 'Nothing' if the run isn't configured to use the disk, or if the current node is configured
-- not to create a folder.
getCurrentFolder :: (HasBaseContext context, MonadReader context m, MonadIO m) => m (Maybe FilePath)
getCurrentFolder = asks (baseContextPath . getBaseContext)

-- | Get the command line options, if configured.
-- Using the 'runSandwichWithCommandLineArgs' family of main functions will introduce these, or you can
-- introduce them manually
getCommandLineOptions :: (HasCommandLineOptions context a, MonadReader context m, MonadIO m) => m (CommandLineOptions a)
getCommandLineOptions = getContext commandLineOptions

-- | Get the user command line options, if configured.
-- This just calls 'getCommandLineOptions' and pulls out the user options.
getUserCommandLineOptions :: (HasCommandLineOptions context a, MonadReader context m, MonadIO m) => m a
getUserCommandLineOptions = optUserOptions <$> getContext commandLineOptions
