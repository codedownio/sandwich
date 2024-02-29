{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Wrapper around 'parallel' for limiting the threads using a semaphore.

module Test.Sandwich.ParallelN (
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  , parallelSemaphore
  , HasParallelSemaphore

  , defaultParallelNodeOptions
  ) where

import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Test.Sandwich.Contexts
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Exception



-- | Wrapper around 'parallel'. Introduces a semaphore to limit the parallelism to N threads.
parallelN :: (
  MonadUnliftIO m
  ) => Int -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelN = parallelN' defaultParallelNodeOptions

parallelN' :: (
  MonadUnliftIO m
  ) => NodeOptions -> Int -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelN' nodeOptions n children = introduce "Introduce parallel semaphore" parallelSemaphore (liftIO $ newQSem n) (const $ return ()) $
  parallel' nodeOptions $ aroundEach "Take parallel semaphore" claimRunSlot children
  where claimRunSlot f = do
          s <- getContext parallelSemaphore
          bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void f)

-- | Same as 'parallelN', but extracts the semaphore size from the command line options.
parallelNFromArgs :: forall context a m. (
  MonadUnliftIO m, HasCommandLineOptions context a
  ) => (CommandLineOptions a -> Int) -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelNFromArgs = parallelNFromArgs' @context @a defaultParallelNodeOptions

parallelNFromArgs' :: forall context a m. (
  MonadUnliftIO m, HasCommandLineOptions context a
  ) => NodeOptions -> (CommandLineOptions a -> Int) -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelNFromArgs' nodeOptions getParallelism children = introduce "Introduce parallel semaphore" parallelSemaphore getQSem (const $ return ()) $
  parallel' nodeOptions $ aroundEach "Take parallel semaphore" claimRunSlot children
  where
    getQSem = do
      n <- getParallelism <$> getContext commandLineOptions
      liftIO $ newQSem n

    claimRunSlot f = do
      s <- getContext parallelSemaphore
      bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void f)

parallelSemaphore :: Label "parallelSemaphore" QSem
parallelSemaphore = Label

type HasParallelSemaphore context = HasLabel context "parallelSemaphore" QSem

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }
