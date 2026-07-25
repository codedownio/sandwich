{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Wrappers around 'parallel' for limiting the threads using a semaphore.

module Test.Sandwich.ParallelN (
  -- * Limiting with a semaphore
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  -- * Limiting with lanes
  , parallelNWithLanes
  , parallelNWithLanes'

  , parallelNWithLanesFromArgs
  , parallelNWithLanesFromArgs'

  , defaultParallelNodeOptions

  -- * Types
  , parallelSemaphore
  , HasParallelSemaphore

  , parallelismLimit
  , HasParallelismLimit
  , ParallelismLimit(..)
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


-- * Types

parallelSemaphore :: Label "parallelSemaphore" QSem
parallelSemaphore = Label

type HasParallelSemaphore context = HasLabel context "parallelSemaphore" QSem

parallelismLimit :: Label "parallelismLimit" ParallelismLimit
parallelismLimit = Label

type HasParallelismLimit context = HasLabel context "parallelismLimit" ParallelismLimit

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }

-- * Functions

-- | Wrapper around 'parallel'. Introduces a semaphore to limit the parallelism to N threads.
--
-- The semaphore is claimed by each individual test in the subtree, so the limit applies to the
-- whole subtree, no matter how deeply nested. It's also available to the tests themselves under
-- the 'parallelSemaphore' label, so you can claim it yourself in specs that this function doesn't
-- wrap.
--
-- Since the tests holding the semaphore change over the life of the node, each child gets its own
-- test timer profile. Use 'parallelNWithLanes' if you'd rather have exactly N profiles.
parallelN :: (
  MonadUnliftIO m
  ) => Int -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelN = parallelN' defaultParallelNodeOptions

parallelN' :: (
  MonadUnliftIO m
  )
  -- | Node options
  => NodeOptions
  -- | Number of threads
  -> Int
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m ()
  -> SpecFree context m ()
parallelN' nodeOptions n = parallelN'' nodeOptions (liftIO $ newQSem n)

-- | Same as 'parallelN', but extracts the semaphore size from the command line options.
parallelNFromArgs :: forall context a m. (
  MonadUnliftIO m, HasCommandLineOptions context a
  )
  -- | Callback to extract the semaphore size
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs = parallelNFromArgs' @context @a defaultParallelNodeOptions

parallelNFromArgs' :: forall context a m. (
  MonadUnliftIO m, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the semaphore size
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs' nodeOptions getParallelism = parallelN'' nodeOptions f
  where
    f = getContext commandLineOptions >>= (liftIO . newQSem) . getParallelism

parallelN'' :: (
  MonadUnliftIO m
  )
  => NodeOptions
  -> ExampleT context m QSem
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m ()
  -> SpecFree context m ()
parallelN'' nodeOptions makeQSem children = introduce "Introduce parallel semaphore" parallelSemaphore makeQSem (const $ return ()) $
  parallel' nodeOptions $ aroundEach "Take parallel semaphore" claimRunSlot children
  where
    claimRunSlot f = do
      s <- getContext parallelSemaphore
      bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void f)

-- | Wrapper around 'parallel' which runs at most N of its own children at a time. Each child runs
-- in one of N lanes, claimed just before it starts and released once it's finished, and the
-- children sharing a lane share a test timer profile. So you get N profiles rather than one per
-- child, which is much easier to read in the speedscope viewer.
--
-- Note that this bounds the children of this node only, which is a different thing from what
-- 'parallelN' bounds:
--
--   * Setup nodes inside a child count against the limit, since a lane is held for the child's
--     whole lifetime.
--
--   * 'parallel' nodes nested below a child are /not/ bounded. If your tree is assembled from
--     nested parallel nodes (for example by 'Test.Sandwich.TH.getSpecFromFolder' with a parallel
--     combiner), use 'parallelN' instead, or the limit will only apply to the top level.
parallelNWithLanes :: (
  Monad m
  ) => Int -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m () -> SpecFree context m ()
parallelNWithLanes = parallelNWithLanes' defaultParallelNodeOptions

parallelNWithLanes' :: (
  Monad m
  )
  -- | Node options
  => NodeOptions
  -- | Number of lanes
  -> Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanes' nodeOptions n = parallelNWithLanes'' nodeOptions (pure n)

-- | Same as 'parallelNWithLanes', but extracts the number of lanes from the command line options.
parallelNWithLanesFromArgs :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Callback to extract the number of lanes
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanesFromArgs = parallelNWithLanesFromArgs' @context @a defaultParallelNodeOptions

parallelNWithLanesFromArgs' :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the number of lanes
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanesFromArgs' nodeOptions getParallelism =
  parallelNWithLanes'' nodeOptions (getParallelism <$> getContext commandLineOptions)

parallelNWithLanes'' :: (
  Monad m
  )
  => NodeOptions
  -> ExampleT context m Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanes'' nodeOptions getLimit children =
  -- Introducing a 'ParallelismLimit' is picked up by the 'parallel' node just below, which uses it
  -- to run at most N children at a time (and to share test timer profiles between them). The node
  -- clears it for its own children, so a nested 'parallel' doesn't claim it a second time.
  introduce "Introduce parallelism limit" parallelismLimit (ParallelismLimit <$> getLimit) (const $ return ()) $
    parallel' nodeOptions children
