-- | Functions for introducing Hedgehog tests into a Sandwich test tree. Modelled after Hspec's version.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/extensions/sandwich-hedgehog here>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Hedgehog (
  -- * Introducing Hedgehog args
  -- Any tests that use Hedgehog should be wrapped in one of these.
  introduceHedgehog
  , introduceHedgehog'
  , introduceHedgehog''

  -- * Versions that can be configured with built-in command line arguments.
  -- Pass --print-quickcheck-flags to list them.
  -- , introduceHedgehogCommandLineOptions
  -- , introduceHedgehogCommandLineOptions'
  -- , introduceHedgehogCommandLineOptions''

  -- * Prop
  , prop

  -- * Modifying Hedgehog args
  , modifyArgs
  -- , modifyMaxSuccess
  -- , modifyMaxDiscardRatio
  -- , modifyMaxSize
  -- , modifyMaxShrinks
  ) where

import Control.Exception.Safe
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Hedgehog as H
import Hedgehog.Internal.Config (UseColor (EnableColor), detectColor)
import Hedgehog.Internal.Property hiding (Label)
import Hedgehog.Internal.Report as H
import Hedgehog.Internal.Runner as HR
import Hedgehog.Internal.Seed as Seed
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Hedgehog.Render
import Test.Sandwich.Internal

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data HedgehogParams = HedgehogParams {
  hedgehogDiscardLimit :: Maybe DiscardLimit
  , hedgehogShrinkLimit :: Maybe ShrinkLimit
  , hedgehogShrinkRetries :: Maybe ShrinkRetries
  , hedgehogTerminationCriteria :: Maybe TerminationCriteria
  , hedgehogConfidence :: Maybe Confidence

  , hedgehogSize :: Maybe Size
  , hedgehogSeed :: Maybe Seed
  } deriving (Show)

defaultHedgehogParams = HedgehogParams {
  hedgehogDiscardLimit = Nothing
  , hedgehogShrinkLimit = Nothing
  , hedgehogShrinkRetries = Nothing
  , hedgehogTerminationCriteria = Nothing
  , hedgehogConfidence = Nothing

  , hedgehogSize = Nothing
  , hedgehogSeed = Nothing
  }

newtype HedgehogContext = HedgehogContext HedgehogParams
  deriving Show
hedgehogContext = Label :: Label "hedgehogContext" HedgehogContext
type HasHedgehogContext context = HasLabel context "hedgehogContext" HedgehogContext

-- | Same as 'introduceHedgehog'' but with default args.
introduceHedgehog :: (MonadIO m, MonadBaseControl IO m)
  => SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog = introduceHedgehog'' "Introduce Hedgehog context" defaultHedgehogParams

-- | Same as 'introduceHedgehog''' but with a default message.
introduceHedgehog' :: (MonadIO m, MonadBaseControl IO m)
  => HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog' = introduceHedgehog'' "Introduce Hedgehog context"

-- | Introduce Hedgehog args with configurable message.
introduceHedgehog'' :: (MonadIO m, MonadBaseControl IO m)
  => String -> HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog'' msg params = introduce msg hedgehogContext (return $ HedgehogContext params) (const $ return ())


-- -- | Same as 'introduceHedgehogCommandLineOptions'' but with default args.
-- introduceHedgehogCommandLineOptions :: forall a m context. (MonadIO m, MonadBaseControl IO m, HasLabel context "commandLineOptions" (CommandLineOptions a), MonadReader context m)
--   => SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
-- introduceHedgehogCommandLineOptions = introduceHedgehogCommandLineOptions'' @a "Introduce Hedgehog context with command line options" stdArgs

-- -- | Same as 'introduceHedgehogCommandLineOptions''' but with a default message.
-- introduceHedgehogCommandLineOptions' :: forall a m context. (MonadIO m, MonadBaseControl IO m, HasLabel context "commandLineOptions" (CommandLineOptions a), MonadReader context m)
--   => Args -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
-- introduceHedgehogCommandLineOptions' = introduceHedgehogCommandLineOptions'' @a "Introduce Hedgehog context with command line options"

-- -- | Introduce Hedgehog args with configurable message, overriding those args with any command line options passed.
-- introduceHedgehogCommandLineOptions'' :: forall a m context. (MonadIO m, MonadBaseControl IO m, HasLabel context "commandLineOptions" (CommandLineOptions a), MonadReader context m)
--   => String -> Args -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
-- introduceHedgehogCommandLineOptions'' msg args = introduce msg hedgehogContext getContext (const $ return ())
--   where
--     getContext = do
--       clo <- getCommandLineOptions @a
--       return $ HedgehogContext $ addCommandLineOptions clo args


-- | Similar to 'it'. Runs the given propery with Hedgehog using the currently introduced 'HedgehogParams'. Throws an appropriate exception on failure.
prop :: (HasCallStack, HasHedgehogContext context, MonadIO m, MonadCatch m) => String -> PropertyT (ExampleT context m) () -> Free (SpecCommand context m) ()
prop msg p = it msg $ do
  HedgehogContext (HedgehogParams {..}) <- getContext hedgehogContext

  let config = PropertyConfig {
        propertyDiscardLimit = fromMaybe (propertyDiscardLimit defaultConfig) hedgehogDiscardLimit
        , propertyShrinkLimit = fromMaybe (propertyShrinkLimit defaultConfig) hedgehogShrinkLimit
        , propertyShrinkRetries = fromMaybe (propertyShrinkRetries defaultConfig) hedgehogShrinkRetries
        , propertyTerminationCriteria = fromMaybe (propertyTerminationCriteria defaultConfig) hedgehogTerminationCriteria
        }

  let size = fromMaybe 0 hedgehogSize
  seed <- maybe Seed.random return hedgehogSeed

  finalReport <- checkReport config size seed p $ \progressReport@(Report {..}) -> do
    progress <- renderProgress EnableColor Nothing progressReport
    debug [i|#{progress}|]

  result <- renderResult EnableColor Nothing finalReport

  widget <- (return . renderHedgehogToImage) =<< ppResult Nothing finalReport
  info [i|Got widget: #{widget}|]

  case reportStatus finalReport of
    H.Failed fr -> throwIO $ Reason (Just callStack) result
    H.GaveUp -> throwIO $ Reason (Just callStack) result
    H.OK -> info [i|#{result}|]

-- | Modify the 'HedgehogParams' for the given spec.
modifyArgs :: (
  HasHedgehogContext context, Monad m
  ) => (HedgehogParams -> HedgehogParams) -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
modifyArgs f = introduce "Modified Hedgehog context" hedgehogContext acquire (const $ return ())
  where
    acquire = do
       HedgehogContext params <- getContext hedgehogContext
       return $ HedgehogContext (f params)

-- -- | Modify the 'maxSuccess' for the given spec.
-- modifyMaxSuccess :: (HasHedgehogContext context, Monad m) => (Int -> Int) -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
-- modifyMaxSuccess f = modifyArgs $ \args -> args { maxSuccess = f (maxSuccess args) }

-- addCommandLineOptions :: CommandLineOptions a -> Args -> Args
-- addCommandLineOptions (CommandLineOptions {optHedgehogOptions=(CommandLineHedgehogOptions {..})}) baseArgs@(Args {..}) = baseArgs {
--   replay = maybe replay (\n -> Just (mkQCGen (fromIntegral n), 0)) optHedgehogSeed
--   , maxDiscardRatio = fromMaybe maxSuccess optHedgehogMaxDiscardRatio
--   , maxSize = fromMaybe maxSuccess optHedgehogMaxSize
--   , maxSuccess = fromMaybe maxSuccess optHedgehogMaxSuccess
--   , maxShrinks = fromMaybe maxSuccess optHedgehogMaxShrinks
--   }

-- * Custom exception type for TUI

data HedgehogException = HedgehogException (H.Report H.Result) String (Maybe CallStack)
  deriving (Show)
instance Exception HedgehogException

formatHedgehogException :: SomeException -> Maybe CustomTUIException
formatHedgehogException e = case fromException e of
  Just (HedgehogException report formatted maybeCallStack) ->
    Just $ CustomTUIExceptionMessageAndCallStack (T.pack formatted) maybeCallStack
  Nothing -> Nothing
