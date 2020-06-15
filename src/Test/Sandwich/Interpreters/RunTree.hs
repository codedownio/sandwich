{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  -- runTreeMain
  runTree
  , RunTreeContext(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.IORef
import Data.Time.Clock
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


waitForTree :: [RunTree] -> IO Result
waitForTree rts = do
  results <- mapM wait (fmap runTreeAsync rts)
  return $ if | any isFailure results -> Failure Nothing (Reason "Some child nodes failed")
              | otherwise -> Success

data RunTreeContext context = RunTreeContext {
  runTreeContext :: Async context
  , runTreeOptions :: Options
  }

-- runTreeMain :: (Show context) => Free (SpecCommand context) () -> ReaderT (RunTreeContext context) IO [RunTree]
-- runTreeMain spec = do
--   [RunTreeGroup {..}] <- runTree (Free (Describe "implicit outer describe" spec (Pure ())))
--   return runTreeChildren


runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (RunTreeContext context) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  rtc@RunTreeContext {..} <- ask

  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext

    startTime <- getCurrentTime
    atomicWriteIORef status (Running startTime)

    (try $ f ctx) >>= \case
      Left (e :: SomeException) -> do
        let maybeLoc = Nothing
        endTime <- getCurrentTime
        atomicWriteIORef status (Done startTime endTime (Failure maybeLoc (Error (Just "Exception in before handler") e)))
        throwIO e
      Right () -> do
        endTime <- getCurrentTime
        atomicWriteIORef status (Done startTime endTime Success)
    return ctx

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ async $ do
    mapM_ wait (fmap runTreeAsync subtree)
    return Success

  let tree = RunTreeGroup l status True subtree myAsync
  rest <- runTree next
  return (tree : rest)


runTree (Free (After l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  RunTreeContext {..} <- ask

  subtree <- runTree subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    ctx <- wait runTreeContext

    startTime <- getCurrentTime
    atomicWriteIORef status (Running startTime)

    (try $ f ctx) >>= \case
      Left (e :: SomeException) -> do
        let maybeLoc = Nothing
        endTime <- getCurrentTime
        let ret = Failure maybeLoc (Error (Just "Exception in after handler") e)
        atomicWriteIORef status (Done startTime endTime ret)
        return ret
      Right () -> do
        endTime <- getCurrentTime
        let ret = Success
        atomicWriteIORef status (Done startTime endTime ret)
        return ret

  let tree = RunTreeGroup l status True subtree myAsync
  rest <- runTree next
  return (tree : rest)


-- runTree (Free (Introduce l alloc cleanup subspec next)) = do
--   (sem, ctxAsync) <- ask

--   allocStatus <- liftIO $ newIORef NotStarted
--   allocJob <- liftIO $ async $ do
--     ctx <- wait ctxAsync
--     withSemaphore sem $ do
--       startTime <- getCurrentTime
--       atomicWriteIORef allocStatus (Running startTime)
--       ctx' <- alloc ctx
--       atomicWriteIORef allocStatus (Done undefined)
--       return ctx'

--   allocJob2 <- liftIO $ async $ do
--     wait allocJob
--     return Success

--   subtree <- withReaderT (const (sem, allocJob)) $ runTree subspec

--   cleanupJob <- undefined
--   overallJob <- undefined
  
--   return $ RunningTreeIntroduce { runningTreeLabel = l
--                                 , runningTreeAllocJob = (allocStatus, allocJob2)
--                                 , runningTreeCleanupJob = cleanupJob
--                                 , runningTreeChildJob = runningTreeJob subtree
--                                 , runningTreeOverallJob = overallJob
--                                 , runningTreeChild = subtree
--                                 }

-- -- runTree indent (Free (DescribeParallel l subspec next)) =

runTree (Free (It l ex next)) = do
  RunTreeContext {..} <- ask
  status <- liftIO $ newIORef NotStarted

  myAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomicWriteIORef status (Running startTime)
    (try $ ex ctx) >>= \case
      Left (e :: SomeException) -> do
        let maybeLoc = Nothing
        endTime <- getCurrentTime
        let ret = Failure maybeLoc (Error (Just "Unknown exception") e)
        atomicWriteIORef status (Done startTime endTime ret)
        return ret
      Right ret -> do
        endTime <- getCurrentTime
        atomicWriteIORef status (Done startTime endTime ret)
        return ret

  let tree = RunTreeSingle l status myAsync

  rest <- runTree next
  return (tree : rest)


runTree (Free (Describe l subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  rtc@RunTreeContext {..} <- ask

  let immediateChildren = getImmediateChildren subspec

  initialContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomicWriteIORef status (Running startTime)
    return ctx

  (mconcat -> subtree, finalAsync) <- flip runStateT initialContextAsync $ do
    forM immediateChildren $ \child -> do
      contextAsync <- get
      tree <- lift $ withReaderT (const $ rtc { runTreeContext = contextAsync }) $ runTree child
      newContextAsync <- liftIO $ async $ do
        _ <- waitForTree tree
        wait runTreeContext
      put newContextAsync
      return tree

  myAsync <- liftIO $ async $ do
    wait finalAsync -- TODO: waitCatch?
    startTime <- getCurrentTime -- TODO
    endTime <- getCurrentTime
    let ret = Success
    atomicWriteIORef status (Done startTime endTime ret)
    return ret
  
  let tree = RunTreeGroup l status False subtree myAsync
  rest <- runTree next
  return (tree : rest)


runTree (Free (DescribeParallel l subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  rtc@RunTreeContext {..} <- ask

  let immediateChildren = getImmediateChildren subspec

  initialContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomicWriteIORef status (Running startTime)
    return ctx

  (mconcat -> subtree, finalAsync) <- flip runStateT initialContextAsync $ do
    forM immediateChildren $ \child -> do
      tree <- lift $ withReaderT (const $ rtc { runTreeContext = runTreeContext }) $ runTree child
      newContextAsync <- liftIO $ async $ do
        waitForTree tree
        wait runTreeContext
      put newContextAsync
      return tree

  myAsync <- liftIO $ async $ do
    wait finalAsync -- TODO: waitCatch?
    startTime <- getCurrentTime -- TODO
    endTime <- getCurrentTime
    let ret = Success
    atomicWriteIORef status (Done startTime endTime ret)
    return ret

  let tree = RunTreeGroup l status False subtree myAsync
  rest <- runTree next
  return (tree : rest)

runTree (Pure _) = return []



getImmediateChildren :: Free (SpecCommand context) () -> [Free (SpecCommand context) ()]
getImmediateChildren (Free (It l ex next)) = (Free (It l ex (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Before l f subspec next)) = (Free (Before l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (After l f subspec next)) = (Free (After l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Introduce l alloc cleanup subspec next)) = (Free (Introduce l alloc cleanup subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Around l f subspec next)) = (Free (Around l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Describe l subspec next)) = (Free (Describe l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (DescribeParallel l subspec next)) = (Free (DescribeParallel l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Pure ()) = [Pure ()]
