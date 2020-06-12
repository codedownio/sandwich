-- |

module Test.Sandwich.Interpreters.RunTree (runTree) where

import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.Example
import qualified Data.List as L

data RunningTree =
  RunningTreeGroup String [RunningTree]
  | RunningTreeItem String (Async Result)



runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT context IO RunningTree
-- runTree indent (Free (Before l subspec next)) =
runTree (Free (Introduce l f subspec next)) = do
  ctx <- ask
  -- job <- liftIO $ async $ ex undefined
  ctx' <- liftIO $ f ctx
  subtree <- withReaderT (const ctx') $ runTree subspec
  return $ RunningTreeGroup "" [subtree]
-- runTree indent (Free (DescribeParallel l subspec next)) =
runTree (Free (It l ex next)) = do
  ctx <- ask
  job <- liftIO $ async $ ex ctx
  return $ RunningTreeItem l job
runTree (Free (Describe l subspec next)) = undefined
runTree (Pure _) = undefined
