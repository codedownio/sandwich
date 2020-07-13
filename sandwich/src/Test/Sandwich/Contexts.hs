{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.Spec

getContext :: (Monad m, HasLabel context l a, HasCallStack, MonadReader context m) => Label l a -> m a
getContext = asks . getLabelValue

getRunRoot :: (Monad m, HasBaseContext context, MonadReader context m) => m (Maybe FilePath)
getRunRoot = asks (baseContextRunRoot . getBaseContext)

getCurrentFolder :: (HasBaseContext context, MonadReader context m, MonadIO m) => m (Maybe FilePath)
getCurrentFolder = asks (baseContextPath . getBaseContext)
