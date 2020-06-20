{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import GHC.Stack
import Test.Sandwich.Types.Spec

getContext :: (HasLabel context l a, HasCallStack) => Label l a -> ExampleM context a
getContext = asks . getLabelValue

getRunRoot :: (HasBaseContext context) => ExampleM context (Maybe FilePath)
getRunRoot = do
  ctx <- ask
  let BaseContext {..} = getBaseContext ctx
  return baseContextRunRoot

getCurrentFolder :: (HasBaseContext context) => ExampleM context (Maybe FilePath)
getCurrentFolder = undefined
