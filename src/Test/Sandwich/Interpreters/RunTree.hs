{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  runTreeMain
  , RunTreeContext(..)
  , getImmediateChildren
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Sequence hiding ((:>))
import Data.String.Interpolate
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

runTreeMain :: Free (SpecCommand context) () -> ReaderT (RunTreeContext context) IO [RunTree]
runTreeMain spec = do
  [RunTreeGroup {..}] <- runTree (Free (Describe "implicit outer describe" spec (Pure ())))
  return runTreeChildren


runTree :: Free (SpecCommand context) r -> ReaderT (RunTreeContext context) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  (status, logs, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (handleAsyncException status) $ unmask $ do
      ctx <- wait runTreeContext
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)

      eitherResult <- tryAny $ f ctx

      let ret = either (\e -> Failure Nothing (GotException (Just "Exception in before handler") (SomeExceptionWithEq e))) (const Success) eitherResult
      endTime <- getCurrentTime
      atomically $ writeTVar status (Done startTime endTime ret)

      case eitherResult of
        Left e -> throwIO e
        Right _ -> return ctx

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (handleAsyncException status) $ unmask $ do
      mapM_ wait (fmap runTreeAsync subtree)
      return Success

  continueWith (RunTreeGroup l status True subtree logs myAsync) next
runTree (Free (After l f subspec next)) = do
  (status, logs, RunTreeContext {..}) <- getInfo

  subtree <- runTree subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)

    (tryAny $ f ctx) >>= \case
      Left e -> do
        let maybeLoc = Nothing
        endTime <- getCurrentTime
        let ret = Failure maybeLoc (GotException (Just "Exception in after handler") (SomeExceptionWithEq e))
        atomically $ writeTVar status (Done startTime endTime ret)
        return ret
      Right () -> do
        endTime <- getCurrentTime
        let ret = Success
        atomically $ writeTVar status (Done startTime endTime ret)
        return ret

  continueWith (RunTreeGroup l status True subtree logs myAsync) next
runTree (Free (Around l f subspec next)) = do
  (status, logs, rtc@RunTreeContext {..}) <- getInfo

  -- Use mvar to control when subspec is allowed to run
  mvar :: MVar () <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ do
    ret <- wait runTreeContext
    void $ takeMVar mvar
    return ret

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)

    eitherResult <- tryAny $ f ctx $ do
      putMVar mvar ()
      void $ waitForTree subtree

    endTime <- getCurrentTime

    ret <- case eitherResult of
      Left e -> do
        let maybeLoc = Nothing
        return $ Failure maybeLoc $ GotException (Just "Exception in around handler") (SomeExceptionWithEq e)
      Right _ -> return Success

    atomically $ writeTVar status (Done startTime endTime Success)
    return ret

  continueWith (RunTreeGroup l status True subtree logs myAsync) next
runTree (Free (Introduce l alloc cleanup subspec next)) = do
  (status, logs, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    ctx <- waitOrHandleContextException runTreeContext status

    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)

    eitherResult <- tryAny $ alloc ctx

    let ret = either (\e -> (Failure Nothing (GotException (Just "Exception in introduce allocate handler") (SomeExceptionWithEq e)))) (const Success) eitherResult
    endTime <- getCurrentTime
    atomically $ writeTVar status (Done startTime endTime ret)

    case eitherResult of
      Left e -> throwIO e
      Right intro -> return (intro :> ctx)

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    ctx <- wait newContextAsync

    startTime <- getCurrentTime -- TODO

    (tryAny $ cleanup ctx) >>= \case
      Left e -> do
        let maybeLoc = Nothing
        endTime <- getCurrentTime
        let ret = Failure maybeLoc (GotException (Just "Exception in introduce cleanup handler") (SomeExceptionWithEq e))
        atomically $ writeTVar status (Done startTime endTime ret)
        return ret
      Right () -> do
        endTime <- getCurrentTime
        let ret = Success
        atomically $ writeTVar status (Done startTime endTime ret)
        return ret

  continueWith (RunTreeGroup l status True subtree logs myAsync) next
runTree (Free (It l ex next)) = do
  (status, logs, RunTreeContext {..}) <- getInfo

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (handleAsyncException status) $ unmask $ do
      ctx <- waitOrHandleContextException runTreeContext status
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      eitherResult <- tryAny $ ex ctx
      endTime <- getCurrentTime
      let ret = either (Failure Nothing . (GotException (Just "Unknown exception") . SomeExceptionWithEq)) id eitherResult
      atomically $ writeTVar status (Done startTime endTime ret)
      return ret

  continueWith (RunTreeSingle l status logs myAsync) next
runTree (Free (Describe l subspec next)) = runDescribe False l subspec next
runTree (Free (DescribeParallel l subspec next)) = runDescribe True l subspec next
runTree (Pure _) = return []


runDescribe :: Bool -> String -> Free (SpecCommand a) () -> Free (SpecCommand a) r -> ReaderT (RunTreeContext a) IO [RunTree]
runDescribe parallel l subspec next = do
  (status, logs, rtc@RunTreeContext {..}) <- getInfo

  (mconcat -> subtree) <- flip evalStateT runTreeContext $
    forM (getImmediateChildren subspec) $ \child -> do
      contextAsync <- get
      let asyncToUse = if parallel then runTreeContext else contextAsync
      tree <- lift $ withReaderT (const $ rtc { runTreeContext = asyncToUse }) $ runTree child
      put =<< liftIO (async $ waitForTree tree >> wait runTreeContext)
      return tree

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (handleAsyncException status) $ unmask $ do
      _ <- wait runTreeContext
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      _ <- waitForTree subtree
      endTime <- getCurrentTime
      let ret = Success
      atomically $ writeTVar status (Done startTime endTime ret)
      return ret

  let tree = RunTreeGroup l status False subtree logs myAsync
  rest <- runTree next
  return (tree : rest)


-- * Helpers

continueWith tree next = do
  rest <- runTree next
  return (tree : rest)

handleAsyncException :: TVar Status -> SomeAsyncException -> IO Result
handleAsyncException status e = do
  -- TODO: get start time
  endTime <- getCurrentTime
  let maybeLoc = Nothing
  let ret = Failure maybeLoc (GotAsyncException Nothing (SomeAsyncExceptionWithEq e))
  atomically $ writeTVar status (Done endTime endTime ret)
  return ret

getInfo = do
  status <- liftIO $ newTVarIO NotStarted
  logs <- liftIO $ newTVarIO mempty
  rts <- ask
  return (status, logs, rts)

logMsg logs msg = atomically $ modifyTVar logs (|> msg)

waitOrHandleContextException contextAsync status = do
  (tryAny $ wait contextAsync) >>= \case
    Left e -> do
      let ret = Failure Nothing (GetContextException (SomeExceptionWithEq e))
      endTime <- getCurrentTime
      atomically $ modifyTVar status $ \case
        Running startTime -> (Done startTime endTime ret) -- TODO: make startTime a Maybe
        _ -> (Done endTime endTime ret) -- TODO: make startTime a Maybe
      throwIO e
    Right ctx -> return ctx


  
getImmediateChildren :: Free (SpecCommand context) () -> [Free (SpecCommand context) ()]
getImmediateChildren (Free (It l ex next)) = (Free (It l ex (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Before l f subspec next)) = (Free (Before l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (After l f subspec next)) = (Free (After l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Introduce l alloc cleanup subspec next)) = (Free (Introduce l alloc cleanup subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Around l f subspec next)) = (Free (Around l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Describe l subspec next)) = (Free (Describe l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (DescribeParallel l subspec next)) = (Free (DescribeParallel l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Pure ()) = [Pure ()]
