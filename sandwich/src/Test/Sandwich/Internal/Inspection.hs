
module Test.Sandwich.Internal.Inspection where

import Control.Monad
import Control.Monad.Logger
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import Test.Sandwich.Internal.Running
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.PruneTree
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.RunTree


getRunTree :: Options -> CoreSpec -> IO [RunNodeFixed BaseContext]
getRunTree options spec = do
  baseContext' <- baseContextFromOptions options
  let baseContext = baseContext' { baseContextPath = Just "/path", baseContextRunRoot = Just "/root" }
  runStderrLoggingT $ getRunTree' baseContext options spec

getRunTree' :: MonadLogger m => BaseContext -> Options -> CoreSpec -> m [RunNodeFixed BaseContext]
getRunTree' baseContext (Options {optionsPruneTree=(unwrapTreeFilter -> pruneOpts), optionsFilterTree=(unwrapTreeFilter -> filterOpts)}) spec =
  spec
    & (\tree -> L.foldl' pruneTree tree pruneOpts)
    & (\tree -> L.foldl' filterTree tree filterOpts)
    & specToRunTreeM baseContext

printRunTree :: [RunNodeFixed ctx] -> IO ()
printRunTree = go 0
  where
    go :: Int -> [RunNodeFixed ctx] -> IO ()
    go depth nodes = do
      forM_ nodes $ \node@(runNodeCommon -> RunNodeCommonWithStatus {..}) -> do
        let spaces = L.replicate (depth * 2) ' '
        putStrLn [i|#{spaces}#{runTreeLabel}, \##{runTreeId} with ancestors #{runTreeAncestors}. Folder: #{runTreeFolder}|]

        case node of
          RunNodeIntroduce {..} -> go (depth + 1) runNodeChildrenAugmented
          RunNodeIntroduceWith {..} -> go (depth + 1) runNodeChildrenAugmented
          RunNodeIt {} -> return ()
          _ -> go (depth + 1) (runNodeChildren node)
