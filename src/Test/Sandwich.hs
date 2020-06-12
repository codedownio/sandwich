
module Test.Sandwich where

import Control.Monad.Free
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Spec

pending _ = return $ Pending Nothing Nothing

pushDownBefores :: Free (SpecCommand context) r -> Free (SpecCommand context) r
pushDownBefores = undefined


