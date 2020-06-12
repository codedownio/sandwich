
module Test.Sandwich where

import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Spec
import Control.Monad.Free
import Test.Sandwich.Interpreters.PrettyShow

pending _ = return $ Result "pending" (Pending Nothing Nothing)


pushDownBefores :: Free (SpecCommand context) r -> Free (SpecCommand context) r
pushDownBefores = undefined


