{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Test.Sandwich.Contexts where

import Control.Monad.Reader
import GHC.Stack
import Test.Sandwich.Types.Spec

askLabel :: (HasLabel context l a, HasCallStack) => Label l a -> ExampleM context a
askLabel = asks . getLabelValue
