{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import GHC.Stack
import Test.Sandwich.Types.Spec

getContext :: (Monad m, HasLabel context l a, HasCallStack) => Label l a -> ExampleT context m a
getContext = asks . getLabelValue

getRunRoot :: (Monad m, HasBaseContext context) => ExampleT context m (Maybe FilePath)
getRunRoot = do
  ctx <- ask
  let BaseContext {..} = getBaseContext ctx
  return baseContextRunRoot

getCurrentFolder :: (HasBaseContext context) => ExampleT context m (Maybe FilePath)
getCurrentFolder = undefined
