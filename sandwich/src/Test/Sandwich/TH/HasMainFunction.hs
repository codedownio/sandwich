{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Sandwich.TH.HasMainFunction
  ( fileHasMainFunction
  , ShouldWarnOnParseError (..)
  ) where

import Control.Monad
import Data.Char
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GHC
import GHC.Types.Name
import GHC.Unit.Module.ModSummary
import Safe
import System.Directory
import System.FilePath as F
import Test.Sandwich.TH.ModuleMap
import Test.Sandwich.Types.Spec hiding (location)
import "template-haskell" Language.Haskell.TH hiding (Name)

data ShouldWarnOnParseError = WarnOnParseError | NoWarnOnParseError
  deriving (Eq)

-- | Use haskell-src-exts to determine if a give Haskell file has an exported main function
-- Parse with all extensions enabled, which will hopefully parse anything
fileHasMainFunction :: FilePath -> ShouldWarnOnParseError -> Q Bool
fileHasMainFunction path shouldWarnOnParseError = do
  hasMain <- runIO $ hasMainFunction path
  case shouldWarnOnParseError of
    WarnOnParseError -> do
      unless hasMain $ reportWarning [i|Warning: File #{path} does not have an exported main function|]
      return hasMain
    NoWarnOnParseError -> return hasMain

hasMainFunction :: FilePath -> IO Bool
hasMainFunction filepath = do
  runGhc Nothing $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    target <- guessTarget filepath Nothing Nothing
    addTarget target
    result <- load LoadAllTargets
    case result of
      Succeeded -> do
        modGraph <- getModuleGraph
        case L.find (\ms -> msHsFilePath ms == filepath) (mgModSummaries modGraph) of
          Nothing -> return False
          Just modSummary -> do
            let modName = moduleNameString (ms_mod_name modSummary)
            mod <- findModule (mkModuleName modName) Nothing
            modInfo <- getModuleInfo mod
            case modInfo of
              Nothing -> return False
              Just mi -> do
                let exports = modInfoExports mi
                return $ any isMainName exports
      Failed -> return False

-- | The arg is Main.
isMainName :: Name -> Bool
isMainName name = getOccString name == "main"
