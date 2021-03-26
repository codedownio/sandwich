{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.TH.HasMainFunction (
  fileHasMainFunction
  ) where

import Data.String.Interpolate
import Language.Haskell.Exts
import Language.Haskell.TH (runIO, reportWarning)

-- import Debug.Trace


-- | Use haskell-src-exts to determine if a give Haskell file has an exported main function
-- Parse with all extensions enabled, which will hopefully parse anything
fileHasMainFunction path = runIO (parseFileWithExts [x | x@(EnableExtension _) <- knownExtensions] path) >>= \case
  x@(ParseFailed {}) -> do
    reportWarning [i|Failed to parse #{path}: #{x}|]
    return False
  ParseOk (Module _ (Just moduleHead) _ _ decls) -> do
    -- traceM [i|Sucessfully parsed #{path}: #{moduleHead}|]
    case moduleHead of
      ModuleHead _ _ _ (Just (ExportSpecList _ (any isMainFunction -> True))) -> do
        -- traceM [i|FOUND MAIN FUNCTION FOR #{path}|]
        return True
      ModuleHead _ _ _ Nothing -> do
        -- traceM [i|LOOKING FOR DECLS: #{decls}|]
        return $ any isMainDecl decls
      _ -> return False
  ParseOk _ -> do
    reportWarning [i|Sucessfully parsed #{path} but no module head found|]
    return False

isMainFunction :: ExportSpec l -> Bool
isMainFunction (EVar _ name) = isMainFunctionQName name
isMainFunction _ = False

isMainFunctionQName :: QName l -> Bool
isMainFunctionQName (Qual _ _ name) = isMainFunctionName name
isMainFunctionQName (UnQual _ name) = isMainFunctionName name
isMainFunctionQName _ = False

isMainFunctionName :: Name l -> Bool
isMainFunctionName (Ident _ "main") = True
isMainFunctionName (Symbol _ "main") = True
isMainFunctionName _ = False

isMainDecl :: (Show l) => Decl l -> Bool
isMainDecl (PatBind _ (PVar _ (Ident _ "main")) _ _) = True
-- isMainDecl decl = trace [i|Looking at decl: #{decl}|] False
isMainDecl _ = False
