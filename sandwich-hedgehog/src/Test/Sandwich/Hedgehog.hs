-- | Functions for introducing Hedgehog tests into a Sandwich test tree. Modelled after Hspec's version.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/extensions/sandwich-hedgehog here>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.Hedgehog (
  -- * Introducing a Hedgehog context
  -- Any tests that use Hedgehog should be wrapped in one of these.
  introduceHedgehog
  , introduceHedgehog'
  , introduceHedgehog''

  -- * Prop
  , prop

  -- * Params
  , HedgehogParams
  , defaultHedgehogParams
  , hedgehogDiscardLimit
  , hedgehogShrinkLimit
  , hedgehogShrinkRetries
  , hedgehogTerminationCriteria
#if MIN_VERSION_hedgehog(1,2,0)
  , hedgehogSkip
#endif
  , hedgehogSize
  , hedgehogSeed

  -- * Versions that can be configured with built-in command line arguments.
  -- Pass --print-hedgehog-flags to list them.
  , introduceHedgehogCommandLineOptions
  , introduceHedgehogCommandLineOptions'
  , introduceHedgehogCommandLineOptions''
  , addCommandLineOptions

  -- * Modifying Hedgehog args
  , modifyArgs
  , modifyDiscardLimit
  , modifyShrinkLimit
  , modifyShrinkRetries
  , modifyTerminationCriteria
#if MIN_VERSION_hedgehog(1,2,0)
  , modifySkip
#endif
  , modifySize
  , modifySeed

  -- * Misc
  , HasHedgehogContext
  ) where

import Control.Applicative
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Interpolate
import GHC.Stack
import Hedgehog as H
import Hedgehog.Internal.Config (UseColor (..))
import Hedgehog.Internal.Property hiding (Label)
import Hedgehog.Internal.Report as H
import Hedgehog.Internal.Runner as HR
import Hedgehog.Internal.Seed as Seed
import Test.Sandwich
import Test.Sandwich.Internal
import UnliftIO.Exception

#ifndef mingw32_HOST_OS
import Test.Sandwich.Hedgehog.Render
#endif


data HedgehogParams = HedgehogParams {
  -- | Random number generator seed.
  hedgehogSeed :: Maybe Seed
  -- | Size of the randomly-generated data.
  , hedgehogSize :: Maybe Size
  -- | The number of times a property is allowed to discard before the test runner gives up.
  , hedgehogDiscardLimit :: Maybe DiscardLimit
  -- | The number of times a property is allowed to shrink before the test runner gives up and prints the counterexample.
  , hedgehogShrinkLimit :: Maybe ShrinkLimit
  -- | The number of times to re-run a test during shrinking.
  , hedgehogShrinkRetries :: Maybe ShrinkRetries
  -- | Control when the test runner should terminate.
  , hedgehogTerminationCriteria :: Maybe TerminationCriteria
  -- | Control where to start running a property's tests
#if MIN_VERSION_hedgehog(1,2,0)
  , hedgehogSkip :: Maybe Skip
#endif
  } deriving (Show)

defaultHedgehogParams :: HedgehogParams
defaultHedgehogParams = HedgehogParams {
  hedgehogSize = Nothing
  , hedgehogSeed = Nothing
  , hedgehogDiscardLimit = Nothing
  , hedgehogShrinkLimit = Nothing
  , hedgehogShrinkRetries = Nothing
  , hedgehogTerminationCriteria = Nothing
#if MIN_VERSION_hedgehog(1,2,0)
  , hedgehogSkip = Nothing
#endif
  }

newtype HedgehogContext = HedgehogContext HedgehogParams
  deriving Show
hedgehogContext :: Label "hedgehogContext" HedgehogContext
hedgehogContext = Label
type HasHedgehogContext context = HasLabel context "hedgehogContext" HedgehogContext

-- | Same as 'introduceHedgehog'' but with default 'HedgehogParams'.
introduceHedgehog :: (MonadIO m)
  => SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog = introduceHedgehog'' "Introduce Hedgehog context" defaultHedgehogParams

-- | Same as 'introduceHedgehog''' but with a default message.
introduceHedgehog' :: (MonadIO m)
  => HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog' = introduceHedgehog'' "Introduce Hedgehog context"

-- | Introduce 'HedgehogParams' with configurable message.
introduceHedgehog'' :: (MonadIO m)
  => String -> HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehog'' msg params = introduce msg hedgehogContext (return $ HedgehogContext params) (const $ return ())


-- | Same as 'introduceHedgehogCommandLineOptions'' but with default 'HedgehogParams'.
introduceHedgehogCommandLineOptions :: forall a m context. (MonadIO m, HasLabel context "commandLineOptions" (CommandLineOptions a))
  => SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehogCommandLineOptions = introduceHedgehogCommandLineOptions'' @a "Introduce Hedgehog context with command line options" defaultHedgehogParams

-- | Same as 'introduceHedgehogCommandLineOptions''' but with a default message.
introduceHedgehogCommandLineOptions' :: forall a m context. (MonadIO m, HasLabel context "commandLineOptions" (CommandLineOptions a))
  => HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehogCommandLineOptions' = introduceHedgehogCommandLineOptions'' @a "Introduce Hedgehog context with command line options"

-- | Introduce 'HedgehogParams' with configurable message, overriding those parameters with any command line options passed.
introduceHedgehogCommandLineOptions'' :: forall a m context. (MonadIO m, HasLabel context "commandLineOptions" (CommandLineOptions a))
  => String -> HedgehogParams -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
introduceHedgehogCommandLineOptions'' msg args = introduce msg hedgehogContext getContext' (const $ return ())
  where
    getContext' = do
      clo <- getCommandLineOptions @a
      return $ HedgehogContext $ addCommandLineOptions clo args


-- | Similar to 'it'. Runs the given propery with Hedgehog using the currently introduced 'HedgehogParams'. Throws an appropriate exception on failure.
prop :: (HasCallStack, HasHedgehogContext context, MonadIO m, MonadCatch m) => String -> PropertyT (ExampleT context m) () -> Free (SpecCommand context m) ()
prop msg p = it msg $ do
  HedgehogContext (HedgehogParams {..}) <- getContext hedgehogContext

  let config = PropertyConfig {
        propertyDiscardLimit = fromMaybe (propertyDiscardLimit defaultConfig) hedgehogDiscardLimit
        , propertyShrinkLimit = fromMaybe (propertyShrinkLimit defaultConfig) hedgehogShrinkLimit
        , propertyShrinkRetries = fromMaybe (propertyShrinkRetries defaultConfig) hedgehogShrinkRetries
        , propertyTerminationCriteria = fromMaybe (propertyTerminationCriteria defaultConfig) hedgehogTerminationCriteria

#if MIN_VERSION_hedgehog(1,2,0)
        , propertySkip = hedgehogSkip <|> propertySkip defaultConfig
#endif

        }

  let size = fromMaybe 0 hedgehogSize
  seed <- maybe Seed.random return hedgehogSeed

  finalReport <- checkReport config size seed p $ \progressReport@(Report {}) -> do
    -- image <- (return . renderHedgehogToImage) =<< ppProgress Nothing progressReport

    progress <- renderProgress DisableColor Nothing progressReport
    debug [i|#{progress}|]

#ifdef mingw32_HOST_OS
  result <- renderResult EnableColor Nothing finalReport
  case reportStatus finalReport of
    H.Failed fr -> throwIO $ Reason (Just callStack) result
    H.GaveUp -> throwIO $ Reason (Just callStack) result
    H.OK -> info [i|#{result}|]
#else
  image <- (return . renderHedgehogToImage) =<< ppResult Nothing finalReport

  -- Hedgehog naturally indents everything by 2. Remove this for the fallback text.
  resultText <- dedent 2 <$> renderResult EnableColor Nothing finalReport
  case reportStatus finalReport of
    H.Failed _ -> throwIO $ RawImage (Just callStack) resultText image
    H.GaveUp -> throwIO $ RawImage (Just callStack) resultText image
    H.OK -> info [i|#{resultText}|]
#endif

-- | Modify the 'HedgehogParams' for the given spec.
modifyArgs :: (
  HasHedgehogContext context, Monad m
  ) => (HedgehogParams -> HedgehogParams) -> SpecFree (LabelValue "hedgehogContext" HedgehogContext :> context) m () -> SpecFree context m ()
modifyArgs f = introduce "Modified Hedgehog context" hedgehogContext acquire (const $ return ())
  where
    acquire = do
       HedgehogContext params <- getContext hedgehogContext
       return $ HedgehogContext (f params)

type HedgehogContextLabel context = LabelValue "hedgehogContext" HedgehogContext :> context

-- | Modify the 'Seed' for the given spec.
modifySeed :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe Seed -> Maybe Seed) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifySeed f = modifyArgs $ \args -> args { hedgehogSeed = f (hedgehogSeed args) }

-- | Modify the 'Size' for the given spec.
modifySize :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe Size -> Maybe Size) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifySize f = modifyArgs $ \args -> args { hedgehogSize = f (hedgehogSize args) }

-- | Modify the 'DiscardLimit' for the given spec.
modifyDiscardLimit :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe DiscardLimit -> Maybe DiscardLimit) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifyDiscardLimit f = modifyArgs $ \args -> args { hedgehogDiscardLimit = f (hedgehogDiscardLimit args) }

-- | Modify the 'ShrinkLimit' for the given spec.
modifyShrinkLimit :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe ShrinkLimit -> Maybe ShrinkLimit) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifyShrinkLimit f = modifyArgs $ \args -> args { hedgehogShrinkLimit = f (hedgehogShrinkLimit args) }

-- | Modify the 'ShrinkRetries' for the given spec.
modifyShrinkRetries :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe ShrinkRetries -> Maybe ShrinkRetries) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifyShrinkRetries f = modifyArgs $ \args -> args { hedgehogShrinkRetries = f (hedgehogShrinkRetries args) }

-- | Modify the 'TerminationCriteria' for the given spec.
modifyTerminationCriteria :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe TerminationCriteria -> Maybe TerminationCriteria) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifyTerminationCriteria f = modifyArgs $ \args -> args { hedgehogTerminationCriteria = f (hedgehogTerminationCriteria args) }

#if MIN_VERSION_hedgehog(1,2,0)
-- | Modify the 'Skip' for the given spec.
modifySkip :: (
  HasHedgehogContext context, Monad m
  ) => (Maybe Skip -> Maybe Skip) -> SpecFree (HedgehogContextLabel context) m () -> SpecFree context m ()
modifySkip f = modifyArgs $ \args -> args { hedgehogSkip = f (hedgehogSkip args) }
#endif

addCommandLineOptions :: CommandLineOptions a -> HedgehogParams -> HedgehogParams
addCommandLineOptions (CommandLineOptions {optHedgehogOptions=(CommandLineHedgehogOptions {..})}) baseHedgehogParams@(HedgehogParams {..}) = baseHedgehogParams {
  hedgehogSeed = (read <$> optHedgehogSeed) <|> hedgehogSeed
  , hedgehogSize = (fromIntegral <$> optHedgehogSize) <|> hedgehogSize
  , hedgehogDiscardLimit = (fromIntegral <$> optHedgehogDiscardLimit) <|> hedgehogDiscardLimit
  , hedgehogShrinkLimit = (fromIntegral <$> optHedgehogShrinkLimit) <|> hedgehogShrinkLimit
  , hedgehogShrinkRetries = (fromIntegral <$> optHedgehogShrinkRetries) <|> hedgehogShrinkRetries
  }
