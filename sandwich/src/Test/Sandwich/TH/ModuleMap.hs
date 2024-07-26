
module Test.Sandwich.TH.ModuleMap where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isNothing)
import System.Directory
import System.FilePath


-- Map from qualified import name to path
type ModuleMap = M.Map String String
type ReverseModuleMap = M.Map String String

buildModuleMap :: FilePath -> String -> IO ModuleMap
buildModuleMap baseDir modulePrefix = traverseDir (const True) (\x y -> return $ addModuleToMap baseDir modulePrefix x y) mempty baseDir

addModuleToMap :: FilePath -> String -> ModuleMap -> FilePath -> ModuleMap
addModuleToMap relativeTo modulePrefix mm path@(takeExtension -> ".hs") = case pathParts of
  [] -> mm
  _ -> M.insert moduleName (modulePrefix <> (L.intercalate "." pathParts)) mm
  where
    relativePath = (takeFileName relativeTo) </> (makeRelative relativeTo path)
    pathParts = splitDirectories $ dropExtension relativePath
    baseModuleName = last pathParts
    moduleName = case filter doesNotExist (baseModuleName : [baseModuleName <> show n | n <- [(1 :: Integer)..]]) of
      (x:_) -> x
      _ -> error "Impossible"
    doesNotExist x = isNothing (M.lookup x mm)
addModuleToMap _ _ mm _ = mm

-- | From https://stackoverflow.com/a/51713361/2659595
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath = do
        names <- listDirectory dirPath
        let paths = map (dirPath </>) names
        (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
        state' <- foldM transition state filePaths
        foldM go state' (filter validDir dirPaths)
 in go

-- | From https://hackage.haskell.org/package/extra-1.7.9/docs/src/Control.Monad.Extra.html#partitionM
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)
