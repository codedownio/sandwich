{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.QuickCheck where

import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import GHC.Stack
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.Internal


data QuickCheckContext = QuickCheckContext Args
  deriving Show
quickCheckContext = Label :: Label "quickCheckContext" QuickCheckContext
type HasQuickCheckContext context = HasLabel context "quickCheckContext" QuickCheckContext

introduceQuickCheck :: (HasQuickCheckContext context, MonadIO m, MonadBaseControl IO m)
  => SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
introduceQuickCheck = introduce "Introduce QuickCheck context" quickCheckContext (return $ QuickCheckContext stdArgs) (const $ return ())

introduceQuickCheck' :: (HasQuickCheckContext context, MonadIO m, MonadBaseControl IO m)
  => Args -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
introduceQuickCheck' args = introduce "Introduce QuickCheck context" quickCheckContext (return $ QuickCheckContext args) (const $ return ())

prop :: (HasCallStack) => prop -> ExampleT context m () -> Free (SpecCommand context m) ()
prop = undefined
