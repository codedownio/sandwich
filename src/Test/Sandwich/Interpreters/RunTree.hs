{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  runTree
  , RunTreeContext(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Time.Clock
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


withSemaphore sem = bracket_ (waitQSem sem) (signalQSem sem)

waitMany :: [Async a] -> Async ()
waitMany = undefined

waitForTree :: [RunTree] -> Async ()
waitForTree = undefined


data RunTreeContext context = RunTreeContext {
  runTreeContext :: Async context
  , runTreeOptions :: Options
  }
  
runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (RunTreeContext context) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  rtc@RunTreeContext {..} <- ask

  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    (try $ f ctx) >>= \case
      Left (e :: SomeException) -> do
        let maybeLoc = Nothing
        atomicWriteIORef status (Done (Failure maybeLoc (Error (Just "Exception in before handler") e)))
        throwIO e
      Right () -> atomicWriteIORef status (Done Success)
    return ctx

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec
  
  let tree = RunTreeGroup l status True subtree (waitMany $ fmap runTreeAsync subtree)
  rest <- runTree next
  return (tree : rest)


runTree (Free (After l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted

  RunTreeContext {..} <- ask

  subtree <- runTree subspec

  myAsync <- liftIO $ async $ do
    wait $ waitForTree subtree
    ctx <- wait runTreeContext
    (try $ f ctx) >>= \case
      Left (e :: SomeException) -> do
        let maybeLoc = Nothing
        atomicWriteIORef status (Done (Failure maybeLoc (Error (Just "Exception in after handler") e)))
      Right () -> atomicWriteIORef status (Done Success)

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
        atomicWriteIORef status (Done (Failure maybeLoc (Error (Just "Unknown exception") e)))
      Right ret -> atomicWriteIORef status (Done ret)

  let tree = RunTreeSingle l status myAsync

  rest <- runTree next
  return (tree : rest)


-- runTree (Free (Describe l subspec next)) = do
--   status <- liftIO $ newIORef NotStarted

--   rtc@RunTreeContext {..} <- ask

--   newContextAsync <- liftIO $ async $ do
--     ctx <- wait runTreeContext
--     f ctx
--     return ctx

--   subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

--   let tree = RunTreeGroup l status subtree
--   rest <- runTree next
--   return (tree : rest)



-- runTree (Free (DescribeParallel l subspec next)) = undefined

runTree (Pure _) = return []
