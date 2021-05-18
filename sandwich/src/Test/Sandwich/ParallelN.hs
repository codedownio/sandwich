{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Wrapper around 'parallel' for limiting the threads using a semaphore.

module Test.Sandwich.ParallelN (parallelN) where

import Control.Concurrent.QSem
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Test.Sandwich.Types.Spec
import Test.Sandwich.Contexts



-- | Wrapped around 'parallel'. Introduces a semaphore to limit the parallelism to N threads.
parallelN :: (
  MonadBaseControl IO m, MonadIO m, MonadMask m
  ) => Int -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
parallelN n children = introduceParallelSemaphore n $ parallel $ aroundEach "Take parallel semaphore" claimRunSlot children
  where claimRunSlot f = do
          s <- getContext parallelSemaphore
          bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) (void f)

parallelSemaphore :: Label "parallelSemaphore" QSem
parallelSemaphore = Label

introduceParallelSemaphore :: (
  MonadIO m, MonadBaseControl IO m
  ) => Int -> SpecFree (LabelValue "parallelSemaphore" QSem :> context) m () -> SpecFree context m ()
introduceParallelSemaphore n = introduce "Introduce parallel semaphore" parallelSemaphore (liftIO $ newQSem n) (const $ return ())
