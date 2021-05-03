
-- | Functions for introducing QuickCheck tests into a Sandwich test tree. Modelled after Hspec's version.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/extensions/sandwich-quickcheck here>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.QuickCheck (
  -- * Introducing QuickCheck args
  -- Any tests that use QuickCheck should be wrapped in one of these.
  introduceQuickCheck
  , introduceQuickCheck'
  , introduceQuickCheck''

  -- * Prop
  , prop

  -- * Modifying QuickCheck args
  , modifyArgs
  , modifyMaxSuccess
  , modifyMaxDiscardRatio
  , modifyMaxSize
  , modifyMaxShrinks
  ) where

import Control.Exception.Safe
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck as QC
import Test.Sandwich
import Test.Sandwich.Internal


data QuickCheckContext = QuickCheckContext Args
  deriving Show
quickCheckContext = Label :: Label "quickCheckContext" QuickCheckContext
type HasQuickCheckContext context = HasLabel context "quickCheckContext" QuickCheckContext

data QuickCheckException = QuickCheckException
  deriving (Show)
instance Exception QuickCheckException

-- | Same as 'introduceQuickCheck'' but with default args.
introduceQuickCheck :: (MonadIO m, MonadBaseControl IO m)
  => SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
introduceQuickCheck = introduceQuickCheck'' "Introduce QuickCheck context" stdArgs

-- | Same as 'introduceQuickCheck''' but with a default message.
introduceQuickCheck' :: (MonadIO m, MonadBaseControl IO m)
  => Args -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
introduceQuickCheck' = introduceQuickCheck'' "Introduce QuickCheck context"

-- | Introduce QuickCheck args with configurable message.
introduceQuickCheck'' :: (MonadIO m, MonadBaseControl IO m)
  => String -> Args -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
introduceQuickCheck'' msg args = introduce msg quickCheckContext (return $ QuickCheckContext args) (const $ return ())

-- | Similar to 'it'. Runs the given prop with QuickCheck using the currently introduced 'Args'. Throws an appropriate exception on failure.
prop :: (HasCallStack, HasQuickCheckContext context, MonadIO m, MonadThrow m, Testable prop) => String -> prop -> Free (SpecCommand context m) ()
prop msg p = it msg $ do
  QuickCheckContext args <- getContext quickCheckContext
  liftIO (quickCheckWithResult (args { QC.chatty = False }) p) >>= \case
    QC.Success {..} -> info (T.pack output)
    x -> throwIO $ Reason (Just callStack) (output x)

-- | Modify the 'Args' for the given spec.
modifyArgs :: (HasQuickCheckContext context, Monad m) => (Args -> Args) -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
modifyArgs f = introduce "Modified QuickCheck context" quickCheckContext acquire (const $ return ())
  where
    acquire = do
       QuickCheckContext args <- getContext quickCheckContext
       return $ QuickCheckContext (f args)

-- | Modify the 'maxSuccess' for the given spec.
modifyMaxSuccess :: (HasQuickCheckContext context, Monad m) => (Int -> Int) -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
modifyMaxSuccess f = modifyArgs $ \args -> args { maxSuccess = f (maxSuccess args) }

-- | Modify the 'maxDiscardRatio' for the given spec.
modifyMaxDiscardRatio :: (HasQuickCheckContext context, Monad m) => (Int -> Int) -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
modifyMaxDiscardRatio f = modifyArgs $ \args -> args { maxDiscardRatio = f (maxDiscardRatio args) }

-- | Modify the 'maxSize' for the given spec.
modifyMaxSize :: (HasQuickCheckContext context, Monad m) => (Int -> Int) -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
modifyMaxSize f = modifyArgs $ \args -> args { maxSize = f (maxSize args) }

-- | Modify the 'maxShrinks' for the given spec.
modifyMaxShrinks :: (HasQuickCheckContext context, Monad m) => (Int -> Int) -> SpecFree (LabelValue "quickCheckContext" QuickCheckContext :> context) m () -> SpecFree context m ()
modifyMaxShrinks f = modifyArgs $ \args -> args { maxShrinks = f (maxShrinks args) }
