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
import Test.Sandwich.TH.ModuleMap
import Test.Sandwich.Types.Spec hiding (location)


-- | The default 'getSpecIndividualSpecHooks'. Takes a 'FilePath' rather than any @b@ so the
-- file name 'getSpecFromFolder' passes in doesn't need defaulting under @OverloadedStrings@.
ignoreFileName :: FilePath -> a -> a
ignoreFileName = const id

data GetSpecFromFolderOptions = GetSpecFromFolderOptions {
  getSpecCombiner :: Name
  , getSpecIndividualSpecHooks :: Name
  , getSpecWarnOnParseError :: ShouldWarnOnParseError
  }

{-# DEPRECATED getSpecWarnOnParseError "Not used anymore." #-}

defaultGetSpecFromFolderOptions :: GetSpecFromFolderOptions
defaultGetSpecFromFolderOptions = GetSpecFromFolderOptions {
  getSpecCombiner = 'describe
  , getSpecIndividualSpecHooks = 'ignoreFileName
  , getSpecWarnOnParseError = WarnOnParseError
  }

{-# DEPRECATED ShouldWarnOnParseError "Not used anymore." #-}
data ShouldWarnOnParseError = WarnOnParseError | NoWarnOnParseError
  deriving (Eq)

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
               maybeMainFunction <- getMaybeMainFunction importedName
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
    Just importedName -> getMaybeMainFunction importedName
  alterNodeOptionsFn <- [e|(\x -> x { nodeOptionsModuleInfo = Just ($(conE 'NodeModuleInfo) currentModule $(return maybeMainFunction)) })|]
  [e|$(varE 'alterTopLevelNodeOptions) $(return alterNodeOptionsFn)
     $ $(varE getSpecCombiner) $(stringE $ mangleFolderName folder) (L.foldl (>>) (pure ()) $(listE $ fmap return specs))|]

-- * Util

-- | Detect if a main function is present in the given module by trying to reify it,
-- using 'recover' to handle exceptions.
-- We could also look at the actual reified value to see if it looks like a function,
-- but that's probably overkill.
getMaybeMainFunction :: String -> Q Exp
getMaybeMainFunction importedName = do
  let mainName = mkName (importedName <> ".main")
  recover [e|Nothing|]
          (reify mainName >> [e|Just $(varE mainName)|])

mangleFolderName :: String -> String
mangleFolderName = T.unpack . wordify . T.pack . takeBaseName

-- | Convert a string like "TeamTests" to "Team tests", leaving acronyms alone:
-- "LSP" stays "LSP" and "HTTPClient" becomes "HTTP client".
wordify :: T.Text -> T.Text
wordify = T.intercalate " " . capitalizeFirst . fmap (unCamel . T.pack) . splitWords . T.unpack
  where
    unCamel w | T.length w > 1, T.all isUpper w = w
              | otherwise = T.toLower w

capitalizeFirst :: [T.Text] -> [T.Text]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = capitalize x : xs

capitalize :: T.Text -> T.Text
capitalize t | T.length t == 1 = T.toUpper t
capitalize t = toUpper (T.head t) `T.cons` (T.tail t)

-- | Split a camel-cased name into words, treating a run of capitals as one word and
-- starting a new word at a digit.
splitWords :: String -> [String]
splitWords = go . L.groupBy ((==) `on` charClass)
  where
    go (ups@(u:_) : word@(w:_) : rest)
      | charClass u == ClassUpper, charClass w == ClassLower = case ups of
          -- The last capital of a run starts the word after it.
          [_] -> (ups <> word) : go rest
          _ -> L.init ups : (L.last ups : word) : go rest
    go (g : rest) = g : go rest
    go [] = []

data CharClass = ClassUpper | ClassLower | ClassDigit
  deriving (Eq)

charClass :: Char -> CharClass
charClass c | isUpper c = ClassUpper
            | isDigit c = ClassDigit
            | otherwise = ClassLower
