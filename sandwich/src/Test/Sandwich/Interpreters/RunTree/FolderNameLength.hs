{-# LANGUAGE RankNTypes #-}

-- | Shortens node folder names so every node folder path fits within 'optionsMaxFolderPathLength'.

module Test.Sandwich.Interpreters.RunTree.FolderNameLength (fitFolderPaths) where

import Control.Monad.Free
import qualified Data.List as L
import System.FilePath
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


fitFolderPaths :: BaseContext -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
fitFolderPaths (BaseContext {..}) spec = case (optionsMaxFolderPathLength baseContextOptions, baseContextRunRoot) of
  (Just maxLength, Just runRoot) -> fst $ go (maxLength - L.length (runRoot </> "results")) [] (countImmediateFolderChildren spec) spec
  _ -> spec

-- | A chain of folders is a list of (padding length, name length). Each chain gets the largest uniform name cap that
-- fits it, and each node takes the smallest cap of any chain through it, which is returned up the tree.
go :: Int -> [(Int, Int)] -> Int -> Free (SpecCommand context m) () -> (Free (SpecCommand context m) (), Maybe Int)
go available ancestors numSiblings = \case
  Free (It'' loc no l ex next) -> case visit no l 0 (\_ _ -> ((), Nothing)) of
    (no', (), cap) -> continue cap next (Free . It'' loc no' l ex)
  Free (Introduce'' loc no l cl alloc cleanup subspec next) -> case visit no l (countImmediateFolderChildren subspec) (goSub subspec) of
    (no', subspec', cap) -> continue cap next (Free . Introduce'' loc no' l cl alloc cleanup subspec')
  Free (IntroduceWith'' loc no l cl action subspec next) -> case visit no l (countImmediateFolderChildren subspec) (goSub subspec) of
    (no', subspec', cap) -> continue cap next (Free . IntroduceWith'' loc no' l cl action subspec')
  Free (Parallel'' loc no subspec next) -> case visit no "Parallel" (countImmediateFolderChildren subspec) (goSub subspec) of
    (no', subspec', cap) -> continue cap next (Free . Parallel'' loc no' subspec')
  -- Before'', After'', Around'', Describe''
  Free x -> case visit (nodeOptions x) (label x) (countImmediateFolderChildren (subspec x)) (goSub (subspec x)) of
    (no', subspec', cap) -> continue cap (next x) (\next' -> Free (x { nodeOptions = no', subspec = subspec', next = next' }))
  pureM@(Pure _) -> (pureM, Nothing)
  where
    goSub :: Free (SpecCommand ctx m) () -> [(Int, Int)] -> Int -> (Free (SpecCommand ctx m) (), Maybe Int)
    goSub subspec ancestors' numSiblings' = go available ancestors' numSiblings' subspec

    continue cap next mk = case go available ancestors numSiblings next of
      (next', nextCap) -> (mk next', minCap cap nextCap)

    -- Folder nodes start a new level of the chain; other nodes' children share this level.
    visit :: NodeOptions -> String -> Int -> ([(Int, Int)] -> Int -> (s, Maybe Int)) -> (NodeOptions, s, Maybe Int)
    visit no l numChildren goChildren
      | nodeOptionsCreateFolder no = case goChildren chain numChildren of
          (children, childrenCap) -> let cap = minCap (chainCap available chain) childrenCap in (no { nodeOptionsFolderNameCap = cap }, children, cap)
      | otherwise = case goChildren ancestors numSiblings of
          (children, childrenCap) -> (no, children, childrenCap)
      where
        paddingLength = L.length (folderNamePadding numSiblings 0)
        chain = ancestors <> [(paddingLength, L.length (nodeToFolderName l numSiblings 0) - paddingLength)]

minCap :: Maybe Int -> Maybe Int -> Maybe Int
minCap (Just x) (Just y) = Just (min x y)
minCap x Nothing = x
minCap Nothing y = y

minFolderNameCap :: Int
minFolderNameCap = 6

-- | The largest name cap that fits the chain within @available@ ('Nothing' if it fits uncapped), but at least 'minFolderNameCap'.
chainCap :: Int -> [(Int, Int)] -> Maybe Int
chainCap available chain
  | fits Nothing = Nothing
  | otherwise = Just (search minFolderNameCap (L.maximum (minFolderNameCap : fmap snd chain)))
  where
    fits cap = sum [1 + paddingLength + maybe id min cap nameLength | (paddingLength, nameLength) <- chain] <= available

    search lo hi
      | hi - lo <= 1 = lo
      | fits (Just mid) = search mid hi
      | otherwise = search lo mid
      where mid = (lo + hi) `div` 2
