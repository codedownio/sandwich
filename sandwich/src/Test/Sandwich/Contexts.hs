{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Functions for retrieving context information from within tests.

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


-- | Get a context by its label.
getContext :: (HasLabel context l a, MonadReader context m) => Label l a -> m a
getContext = asks . getLabelValue

-- | Get the root folder of the on-disk test tree for the current run.
-- Will be 'Nothing' if the run isn't configured to use the disk.
getRunRoot :: (HasBaseContext context, MonadReader context m) => m (Maybe FilePath)
getRunRoot = asks (baseContextRunRoot . getBaseContext)

-- | Get the on-disk folder corresponding to the current node.
-- Will be 'Nothing' if the run isn't configured to use the disk, or if the current node is configured
-- not to create a folder.
getCurrentFolder :: (HasBaseContext context, MonadReader context m) => m (Maybe FilePath)
getCurrentFolder = asks (baseContextPath . getBaseContext)

-- | Get the command line options, if configured.
-- Using the 'runSandwichWithCommandLineArgs' family of main functions will introduce these, or you can
-- introduce them manually
getCommandLineOptions :: forall a context m. (HasCommandLineOptions context a, MonadReader context m) => m (CommandLineOptions a)
getCommandLineOptions = getContext commandLineOptions

-- | Get the user command line options, if configured.
-- This just calls 'getCommandLineOptions' and pulls out the user options.
getUserCommandLineOptions :: (HasCommandLineOptions context a, MonadReader context m) => m a
getUserCommandLineOptions = optUserOptions <$> getContext commandLineOptions

-- * Low-level context management helpers

-- | Push a label to the context.
pushContext :: forall m l a intro context. Label l intro -> intro -> ExampleT (LabelValue l intro :> context) m a -> ExampleT context m a
pushContext _label value (ExampleT action) = do
  ExampleT $ withReaderT (\context -> LabelValue value :> context) $ action

-- | Remove a label from the context.
popContext :: forall m l a intro context. Label l intro -> ExampleT context m a -> ExampleT (LabelValue l intro :> context) m a
popContext _label (ExampleT action) = do
  ExampleT $ withReaderT (\(_ :> context) -> context) $ action
