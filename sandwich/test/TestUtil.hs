{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module TestUtil where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.String.Interpolate
import GHC.Stack
import System.Exit
import Test.Sandwich
import Test.Sandwich.Internal
import UnliftIO.Exception

-- * Main function

mainWith :: (HasCallStack) => (HasCallStack => WriterT [SomeException] IO ()) -> IO ()
mainWith tests = do
  results <- execWriterT tests

  case results of
    [] -> do
      putStrLn "All tests succeeded!"
    xs -> do
      putStrLn [i|\n\n#{length xs} test(s) failed\n\n|]
      forM_ xs $ \x -> putStrLn [i|#{x}\n\n|]
      exitWith (ExitFailure 1)


-- * Values

data FakeDatabase = FakeDatabase deriving Show
fakeDatabaseLabel :: Label "fakeDatabase" FakeDatabase
fakeDatabaseLabel = Label

someUserError :: IOError
someUserError = userError "Oh no"

someUserErrorWrapped :: SomeExceptionWithEq
someUserErrorWrapped = SomeExceptionWithEq $ SomeException $ userError "Oh no"

-- * Helpers

run :: (MonadIO m) => IO () -> WriterT [SomeException] m ()
run test = liftIO (tryAny test) >>= \case
  Left err -> tell [err]
  Right () -> return ()

throwSomeUserError :: (MonadIO m) => m ()
throwSomeUserError = liftIO $ throwIO someUserError

runAndGetResults :: (HasCallStack) => CoreSpec -> IO [Result]
runAndGetResults spec = do
  finalTree <- runSandwichTree defaultOptions spec
  fixedTree <- atomically $ mapM fixRunTree finalTree
  return $ fmap statusToResult $ concatMap getStatuses fixedTree

runAndGetResultsAndLogs :: (HasCallStack) => CoreSpec -> IO ([Result], [[LogStr]])
runAndGetResultsAndLogs spec = do
  finalTree <- runSandwichTree defaultOptions spec
  getResultsAndMessages <$> fixTree finalTree

fixTree :: [RunNode context] -> IO [RunNodeFixed context]
fixTree rts = atomically $ mapM fixRunTree rts

getResultsAndMessages :: (HasCallStack) => [RunNodeFixed context] -> ([Result], [[LogStr]])
getResultsAndMessages fixedTree = (results, msgs)
  where
    results = fmap statusToResult $ concatMap getStatuses fixedTree
    msgs = getMessages fixedTree

getMessages :: [RunNodeFixed context] -> [[LogStr]]
getMessages fixedTree = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

getStatuses :: RunNodeWithStatus context s l t -> [(String, s)]
getStatuses = extractValues $ \node -> (runTreeLabel $ runNodeCommon node, runTreeStatus $ runNodeCommon node)

getLogs :: RunNodeWithStatus context s l t -> [l]
getLogs = extractValues $ \node -> runTreeLogs $ runNodeCommon node

statusToResult :: (HasCallStack) => (String, Status) -> Result
statusToResult (label, NotStarted) = error [i|Expected status to be Done but was NotStarted for label '#{label}' (#{callStack})|]
statusToResult (label, Running {}) = error [i|Expected status to be Done but was Running for label '#{label}' (#{callStack})|]
statusToResult (_, Done _ _ _ _ result) = result

mustBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
mustBe x y
  | x == y = return ()
  | otherwise = error [i|Expected #{show y} but got #{show x} (#{callStack})|]

waitUntilRunning :: TVar Status -> IO ()
waitUntilRunning status = atomically $ do
  readTVar status >>= \case
    Running {} -> return ()
    _ -> retry

-- printFixedRunTree :: RunNodeFixed -> IO ()
-- printFixedRunTree = printFixedRunTree' 0
--   where
--     printFixedRunTree' :: Int -> RunNodeFixed -> IO ()
--     printFixedRunTree' indent (RunTreeGroup {..}) = do
--       putStrLn [i|#{indentation}#{runTreeLabel} [#{statusResult runTreeStatus}]|]
--       forM_ runTreeChildren (printFixedRunTree' (indent + 1))
--       where indentation = L.replicate (indent * 4) ' '
--     printFixedRunTree' indent (RunTreeSingle {..}) = putStrLn [i|#{indentation}#{runTreeLabel} [#{statusResult runTreeStatus}]|]
--       where indentation = L.replicate (indent * 4) ' '
