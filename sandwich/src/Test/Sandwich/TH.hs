{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Sandwich.TH (
  getSpecFromFolder

  , defaultGetSpecFromFolderOptions
  , GetSpecFromFolderOptions
  , getSpecCombiner
  , getSpecIndividualSpecHooks
  , getSpecWarnOnParseError
  , ShouldWarnOnParseError(..)

  , buildModuleMap
  ) where

import Control.Monad
import Data.Char
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Safe
import System.Directory
import System.FilePath as F
import Test.Sandwich.TH.HasMainFunction
import Test.Sandwich.TH.ModuleMap
import Test.Sandwich.Types.Spec hiding (location)


constId :: b -> a -> a
constId = const id

data GetSpecFromFolderOptions = GetSpecFromFolderOptions {
  getSpecCombiner :: Name
  , getSpecIndividualSpecHooks :: Name
  , getSpecWarnOnParseError :: ShouldWarnOnParseError
  }

defaultGetSpecFromFolderOptions :: GetSpecFromFolderOptions
defaultGetSpecFromFolderOptions = GetSpecFromFolderOptions {
  getSpecCombiner = 'describe
  , getSpecIndividualSpecHooks = 'constId
  , getSpecWarnOnParseError = WarnOnParseError
  }

getSpecFromFolder :: GetSpecFromFolderOptions -> Q Exp
getSpecFromFolder getSpecFromFolderOptions = do
  dir <- runIO getCurrentDirectory
  filename <- loc_filename <$> location
  let folder = dropExtension (dir </> filename)

  Module _ (ModName moduleName) <- thisModule

  let modulePrefix' = moduleName
                    & T.pack
                    & T.splitOn "."
                    & initMay
                    & fromMaybe []
                    & T.intercalate "."
                    & T.unpack
  let modulePrefix = if modulePrefix' == "" then "" else modulePrefix' <> "."
  moduleMap <- runIO $ buildModuleMap folder modulePrefix
  let reverseModuleMap = M.fromList [(y, x) | (x, y) <- M.toList moduleMap]

  getSpecFromFolder' folder reverseModuleMap (moduleName <> ".") getSpecFromFolderOptions

getSpecFromFolder' :: F.FilePath -> ReverseModuleMap -> String -> GetSpecFromFolderOptions -> Q Exp
getSpecFromFolder' folder reverseModuleMap modulePrefix gsfo@(GetSpecFromFolderOptions {..}) = do
  items <- qRunIO $ L.sort <$> listDirectory folder
  specs <- (catMaybes <$>) $ forM items $ \item -> do
    isDirectory <- qRunIO $ doesDirectoryExist (folder </> item)

    if | isDirectory -> do
           qRunIO (doesFileExist (folder </> item <.> "hs")) >>= \case
             False -> Just <$> getSpecFromFolder' (folder </> item) reverseModuleMap (modulePrefix <> item <> ".") gsfo
             True -> return Nothing -- Do nothing, allow the .hs file to be picked up separately
       | takeExtension item == ".hs" -> do
           let fullyQualifiedModule = modulePrefix <> takeBaseName item
           case M.lookup fullyQualifiedModule reverseModuleMap of
             Nothing -> do
               reportError [i|Couldn't find module #{fullyQualifiedModule} in #{reverseModuleMap}|]
               return Nothing
             Just importedName -> do
               maybeMainFunction <- fileHasMainFunction (folder </> item) getSpecWarnOnParseError >>= \case
                 True -> [e|Just $(varE $ mkName $ importedName <> ".main")|]
                 False -> [e|Nothing|]

               alterNodeOptionsFn <- [e|(\x -> x { nodeOptionsModuleInfo = Just ($(conE 'NodeModuleInfo) fullyQualifiedModule $(return maybeMainFunction)) })|]

               Just <$> [e|$(varE 'alterTopLevelNodeOptions) $(return alterNodeOptionsFn)
                           $ $(varE getSpecIndividualSpecHooks) $(stringE item) $(varE $ mkName $ importedName <> ".tests")|]
       | otherwise -> return Nothing

  let currentModule = modulePrefix
                    & T.pack
                    & T.stripSuffix "."
                    & fromMaybe ""
                    & T.unpack
  maybeMainFunction <- case M.lookup currentModule reverseModuleMap of
    Nothing -> [e|Nothing|]
    Just importedName -> fileHasMainFunction (folder <> ".hs") getSpecWarnOnParseError >>= \case
      True -> [e|Just $(varE $ mkName $ importedName <> ".main")|]
      False -> [e|Nothing|]
  alterNodeOptionsFn <- [e|(\x -> x { nodeOptionsModuleInfo = Just ($(conE 'NodeModuleInfo) currentModule $(return maybeMainFunction)) })|]
  [e|$(varE 'alterTopLevelNodeOptions) $(return alterNodeOptionsFn)
     $ $(varE getSpecCombiner) $(stringE $ mangleFolderName folder) (L.foldl (>>) (pure ()) $(listE $ fmap return specs))|]

-- * Util

mangleFolderName :: String -> String
mangleFolderName = T.unpack . wordify . T.pack . takeBaseName

-- | Convert a string like "TeamTests" to "Team tests"
wordify :: T.Text -> T.Text
wordify t = T.intercalate " " $ capitalizeFirst $ fmap (T.toLower . T.pack) parts
  where parts = splitR (\c -> isUpper c || isDigit c) $ T.unpack t

capitalizeFirst :: [T.Text] -> [T.Text]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = capitalize x : xs

capitalize :: T.Text -> T.Text
capitalize t | T.length t == 1 = T.toUpper t
capitalize t = (toUpper $ T.head t) `T.cons` (T.tail t)

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case L.break p s' of
      (b', [])     -> [ m:b' ]
      (b', x:xs) -> ( m:b' ) : go x xs
  in case L.break p s of
    (b,  [])    -> [ b ]
    ([], h:t) -> go h t
    (b,  h:t) -> b : go h t
