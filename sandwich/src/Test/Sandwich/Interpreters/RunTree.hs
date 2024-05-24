{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Sandwich.Interpreters.RunTree (
  specToRunTree
  , specToRunTreeVariable
  , specToRunTreeM
  , isEmptySpec
  ) where

import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Logger
import Control.Monad.Trans.RWS
import Data.Foldable (toList)
import Data.Functor.Identity
import qualified Data.List as L
import qualified Data.Map as M
import Data.Sequence as Seq
import GHC.Stack
import Lens.Micro
import Lens.Micro.TH
import Safe (headMay)
import System.FilePath
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree (unFixRunTree)
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data CreatedNode = CreatedNode {
  _createdNodeFolder :: Maybe (FilePath, Int, Int)
  } deriving (Show)
makeLenses ''CreatedNode

data ConvertState = ConvertState {
  _convertStateIdCounter :: Int
  , _convertStateCreatedNodes :: M.Map Int CreatedNode
  , _convertStateRootNextChildIndex :: Int
  }
emptyConvertState :: ConvertState
emptyConvertState = ConvertState 0 mempty 0
makeLenses ''ConvertState

specToRunTree :: BaseContext -> Free (SpecCommand BaseContext IO) () -> [RunNodeFixed BaseContext]
specToRunTree baseContext spec = runIdentity $ runNoLoggingT $ specToRunTreeM baseContext spec

specToRunTreeVariable :: BaseContext -> Free (SpecCommand BaseContext IO) () -> STM [RunNode BaseContext]
specToRunTreeVariable bc spec = mapM unFixRunTree $ specToRunTree bc spec

isEmptySpec :: forall context. Free (SpecCommand context IO) () -> Bool
isEmptySpec spec = L.null ret
  where context = RunTreeContext {
          runTreeCurrentAncestors = mempty
          , runTreeRootFolderAndNumChildren = Nothing
          }
        (ret, _, _) = runIdentity $ runNoLoggingT $ runRWST (specToRunTree' spec) context emptyConvertState

specToRunTreeM :: (MonadLogger m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m [RunNodeFixed BaseContext]
specToRunTreeM baseContext spec = do
  let context = RunTreeContext {
        runTreeCurrentAncestors = mempty
        , runTreeRootFolderAndNumChildren = case baseContextRunRoot baseContext of
            Nothing -> Nothing
            Just root -> Just (root </> "results", countImmediateFolderChildren spec)
        }
  (ret, _, _) <- runRWST (specToRunTree' spec) context emptyConvertState
  return ret

-- | Convert a spec to a run tree
specToRunTree' :: (MonadLogger m) => Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
specToRunTree' node@(Free (Before'' loc no l f subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeBefore common <$> recurse l no common subspec <*> pure f
specToRunTree' node@(Free (After'' loc no l f subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeAfter common <$> recurse l no common subspec <*> pure f
specToRunTree' node@(Free (Introduce'' loc no l _cl alloc cleanup subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeIntroduce common <$> recurse l no common subspec <*> pure alloc <*> pure cleanup
specToRunTree' node@(Free (IntroduceWith'' loc no l _cl action subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeIntroduceWith common <$> recurse l no common subspec <*> pure action
specToRunTree' node@(Free (Around'' loc no l actionWith subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeAround common <$> recurse l no common subspec <*> pure actionWith
specToRunTree' node@(Free (Describe'' loc no l subspec next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeDescribe common <$> recurse l no common subspec
specToRunTree' node@(Free (Parallel'' loc no subspec next)) = do
  common <- getCommon "Parallel" loc node no
  continueWith next =<< RunNodeParallel common <$> recurse "Parallel" no common subspec
specToRunTree' node@(Free (It'' loc no l example next)) = do
  common <- getCommon l loc node no
  continueWith next =<< RunNodeIt common <$> pure example
specToRunTree' (Pure _) = return []


-- * Util

type ConvertM m = RWST RunTreeContext () ConvertState m

getCommon :: (MonadLogger m) => String -> Maybe SrcLoc -> Free (SpecCommand context n) a -> NodeOptions -> ConvertM m RunNodeCommonFixed
getCommon l srcLoc node (NodeOptions {..}) = do
  RunTreeContext {..} <- ask

  -- Get a unique ID for this node
  ident <- _convertStateIdCounter <$> get
  modify (over convertStateIdCounter (+1))

  -- Determine the folder for the node
  folder <- case nodeOptionsCreateFolder of
    False -> pure Nothing
    True -> do
      createdNodes <- _convertStateCreatedNodes <$> get
      -- Look up the first ancestor that has a folder
      case headMay [(ancestor, folder) | ancestor@(flip M.lookup createdNodes -> Just (CreatedNode (Just folder)))
                                         <- (toList $ Seq.reverse runTreeCurrentAncestors)] of
        Nothing -> do
          -- No ancestor has a folder, so we have to put this folder under the root
          case runTreeRootFolderAndNumChildren of
            Just (rootFolder, numRootChildren) -> do
              nextRootChild <- _convertStateRootNextChildIndex <$> get
              modify (over convertStateRootNextChildIndex (+1))
              pure $ Just (rootFolder </> nodeToFolderName l numRootChildren nextRootChild)
            Nothing -> pure Nothing
        Just (ancestor, (folder, totalChildren, nextChildIndex)) -> do
          let bumpAncestorNextChildIndex :: M.Map Int CreatedNode -> M.Map Int CreatedNode
              bumpAncestorNextChildIndex = M.adjust (over (createdNodeFolder . _Just . _3) (+1)) ancestor
          modify (over convertStateCreatedNodes bumpAncestorNextChildIndex)
          pure $ Just (folder </> nodeToFolderName l totalChildren nextChildIndex)

  -- Insert this node into the ConvertState
  modify $ over convertStateCreatedNodes $ M.insert ident $ CreatedNode $ case folder of
    Nothing -> Nothing
    Just f -> Just (f, countImmediateFolderChildren node, 0)

  return $ RunNodeCommonWithStatus {
    runTreeLabel = l
    , runTreeId = ident
    , runTreeAncestors = runTreeCurrentAncestors |> ident
    , runTreeToggled = False
    , runTreeOpen = True
    , runTreeStatus = NotStarted
    , runTreeVisible = True
    , runTreeFolder = folder
    , runTreeVisibilityLevel = nodeOptionsVisibilityThreshold
    , runTreeRecordTime = nodeOptionsRecordTime
    , runTreeLogs = mempty
    , runTreeLoc = srcLoc
    }

continueWith :: (MonadLogger m) => Free (SpecCommand context IO) r -> RunNodeFixed context -> ConvertM m [RunNodeFixed context]
continueWith next node = do
  rest <- specToRunTree' next
  return (node : rest)

recurse :: (MonadLogger m) => String -> NodeOptions -> RunNodeCommonFixed -> Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
recurse _ (NodeOptions {}) (RunNodeCommonWithStatus {..}) subspec = local
  (\rtc -> rtc { runTreeCurrentAncestors = runTreeAncestors })
  (specToRunTree' subspec)
