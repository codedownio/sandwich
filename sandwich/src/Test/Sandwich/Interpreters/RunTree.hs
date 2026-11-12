{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Sandwich.Interpreters.RunTree (
  specToRunTree
  , specToRunTreeVariable
  , specToRunTreeM
  , specToRunTreeWithWarnings
  , isEmptySpec
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free
import Control.Monad.Logger
import Control.Monad.Trans.RWS
import Data.Foldable (toList)
import Data.Functor.Identity
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.String.Interpolate
import qualified Data.Text as T
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


data CreatedFolder = CreatedFolder {
  _createdFolderPath :: FilePath
  , _createdFolderNumChildren :: Int
  , _createdFolderNextChildIndex :: Int
  , _createdFolderChain :: FolderChain
  } deriving (Show)
makeLenses ''CreatedFolder

data CreatedNode = CreatedNode {
  _createdNodeFolder :: Maybe CreatedFolder
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
specToRunTree baseContext spec = fst $ specToRunTreeWithWarnings baseContext spec

specToRunTreeVariable :: BaseContext -> Free (SpecCommand BaseContext IO) () -> STM [RunNode BaseContext]
specToRunTreeVariable bc spec = mapM unFixRunTree $ specToRunTree bc spec

-- | Convert a spec to a run tree, also returning any warnings (such as folder paths that couldn't be
-- shortened enough to fit within 'optionsMaxFolderPathLength').
specToRunTreeWithWarnings :: BaseContext -> Free (SpecCommand BaseContext IO) () -> ([RunNodeFixed BaseContext], [String])
specToRunTreeWithWarnings baseContext spec = runIdentity $ runNoLoggingT $ specToRunTreeM' baseContext spec

isEmptySpec :: forall context. Free (SpecCommand context IO) () -> Bool
isEmptySpec spec = L.null ret
  where context = RunTreeContext {
          runTreeCurrentAncestors = mempty
          , runTreeRootFolderAndNumChildren = Nothing
          , runTreeFolderNameCaps = mempty
          , runTreeFolderPathReserve = 0
          }
        (ret, _, _) = runIdentity $ runNoLoggingT $ runRWST (specToRunTree' spec) context emptyConvertState

specToRunTreeM :: (MonadLogger m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m [RunNodeFixed BaseContext]
specToRunTreeM baseContext spec = do
  (ret, warnings) <- specToRunTreeM' baseContext spec
  forM_ warnings (logWarnN . T.pack)
  return ret

specToRunTreeM' :: (MonadLogger m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m ([RunNodeFixed BaseContext], [String])
specToRunTreeM' baseContext spec = do
  let options = baseContextOptions baseContext
  let rootFolderAndNumChildren = case baseContextRunRoot baseContext of
        Nothing -> Nothing
        Just root -> Just (root </> "results", countImmediateFolderChildren spec)
  let context = RunTreeContext {
        runTreeCurrentAncestors = mempty
        , runTreeRootFolderAndNumChildren = rootFolderAndNumChildren
        , runTreeFolderNameCaps = mempty
        , runTreeFolderPathReserve = optionsFolderPathReserve options
        }
  (ret, finalState, _) <- runRWST (specToRunTree' spec) context emptyConvertState

  case (optionsMaxFolderPathLength options, rootFolderAndNumChildren) of
    (Just budget, Just (rootFolder, _)) -> do
      -- Now that we know every folder chain, choose caps so they all fit, and convert again using them.
      -- The second pass walks the spec identically, so node IDs match up with the first.
      let chains = [_createdFolderChain cf | CreatedNode (Just cf) <- M.elems (_convertStateCreatedNodes finalState)]
      let (caps, overflows) = computeFolderNameCaps budget (L.length rootFolder) chains
      if | M.null caps && L.null overflows -> return (ret, [])
         | otherwise -> do
             (ret', _, _) <- runRWST (specToRunTree' spec) (context { runTreeFolderNameCaps = caps }) emptyConvertState
             return (ret', overflowWarnings budget rootFolder overflows)
    _ -> return (ret, [])

overflowWarnings :: Int -> FilePath -> [FolderChain] -> [String]
overflowWarnings _ _ [] = []
overflowWarnings budget rootFolder overflows@(_:_) = [
  [i|#{L.length overflows} test folder path(s) don't fit within #{budget} characters even with every folder name shortened to #{minFolderNameCap} characters. |]
  <> [i|The deepest is #{L.length (folderChainComponents deepest)} folders deep under #{rootFolder} (reserving #{folderChainReserve deepest} characters inside it). |]
  <> [i|Using the shortest names anyway. Set a smaller folder path reserve, or turn off the limit if long paths are enabled on this system.|]
  ]
  where
    deepest = L.maximumBy (comparing (L.length . folderChainComponents)) overflows

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

  let reserve = fromMaybe runTreeFolderPathReserve nodeOptionsFolderPathReserve
  let cap = M.lookup ident runTreeFolderNameCaps

  -- Determine the folder for the node
  folderAndChain <- case nodeOptionsCreateFolder of
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
              pure $ Just (rootFolder </> nodeToFolderNameCapped cap l numRootChildren nextRootChild
                          , FolderChain reserve [folderComponent ident l numRootChildren nextRootChild])
            Nothing -> pure Nothing
        Just (ancestor, CreatedFolder {..}) -> do
          let bumpAncestorNextChildIndex :: M.Map Int CreatedNode -> M.Map Int CreatedNode
              bumpAncestorNextChildIndex = M.adjust (over (createdNodeFolder . _Just . createdFolderNextChildIndex) (+1)) ancestor
          modify (over convertStateCreatedNodes bumpAncestorNextChildIndex)
          pure $ Just (_createdFolderPath </> nodeToFolderNameCapped cap l _createdFolderNumChildren _createdFolderNextChildIndex
                      , FolderChain reserve (folderChainComponents _createdFolderChain
                                             <> [folderComponent ident l _createdFolderNumChildren _createdFolderNextChildIndex]))

  let folder = fst <$> folderAndChain

  -- Insert this node into the ConvertState
  modify $ over convertStateCreatedNodes $ M.insert ident $ CreatedNode $ case folderAndChain of
    Nothing -> Nothing
    Just (f, chain) -> Just (CreatedFolder f (countSubspecFolderChildren node) 0 chain)

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
