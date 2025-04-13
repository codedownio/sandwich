{-# LANGUAGE OverloadedStrings #-}

module Test.Sandwich.TH.HasMainFunction (
  fileHasMainFunction
  , ShouldWarnOnParseError(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH (Q, runIO, reportWarning)


data ShouldWarnOnParseError = WarnOnParseError | NoWarnOnParseError
  deriving (Eq)

fileHasMainFunction :: FilePath -> ShouldWarnOnParseError -> Q Bool
fileHasMainFunction filePath shouldWarnOnParseError = do
  t <- runIO $ T.readFile filePath
  case fileHasMainFunction' t of
    Left err -> do
      when (shouldWarnOnParseError == WarnOnParseError) $
        reportWarning [i|Failed to parse #{filePath}: #{err}|]
      return False
    Right x -> return x

fileHasMainFunction' :: T.Text -> Either String Bool
fileHasMainFunction' source = case parseOnly parser (stripComments source) of
  Right result -> Right $ checkForMain result
  Left err -> Left err
  where
    parser = do
      modDecl <- parseModule
      decls <- parseTopLevelDeclarations
      return (modDecl, decls)

    checkForMain (modDecl, decls) = case modDecl of
      Just (_, exports) -> "main" `elem` exports
      Nothing -> "main" `elem` decls

stripComments :: T.Text -> T.Text
stripComments = removeLineComments . removeBlockComments
  where
    removeLineComments = T.unlines . map stripLineComment . T.lines
    stripLineComment line =
      case T.breakOn "--" line of
        (code, comment) | T.null comment -> code
                        | otherwise      -> code

    removeBlockComments text = case T.breakOn "{-" text of
      (before, after)
        | T.null after -> before
        | otherwise    ->
            case T.breakOn "-}" (T.drop 2 after) of
              (_, afterEnd)
                | T.null afterEnd -> before
                | otherwise       -> removeBlockComments (T.append before (T.drop 2 afterEnd))

parseModule :: Parser (Maybe (T.Text, [T.Text]))
parseModule = do
  _ <- many' (skipSpace *> (satisfy (not . isAlpha)))
  _ <- skipSpace
  moduleStr <- string "module" <|> return ""
  if moduleStr == "module"
    then do
      skipSpace
      moduleName <- parseIdentifier
      skipSpace
      exports <- option [] $ parens parseExports
      return $ Just (moduleName, exports)
    else return Nothing

-- | Parse a list of exported identifiers
parseExports :: Parser [T.Text]
parseExports = sepBy' parseExportItem (skipSpace *> char ',' <* skipSpace)

-- | Parse a single export item (could be a function, type, etc.)
parseExportItem :: Parser T.Text
parseExportItem = do
  item <- parseIdentifier
  -- Ignore type constructors or other details
  _ <- option "" (parens $ many' $ notChar ')')
  return item

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseIdentifier :: Parser T.Text
parseIdentifier = do
  first <- satisfy (\c -> isAlpha c || c == '_')
  rest <- many' (satisfy (\c -> isAlphaNum c || c == '_' || c == '\''))
  return $ T.pack (first : rest)

parseTopLevelDeclarations :: Parser [T.Text]
parseTopLevelDeclarations = many' $ do
  skipSpace
  name <- parseIdentifier
  skipSpace
  _ <- many' (satisfy (\c -> c == ':' || c == '-' || c == '>' || isSpace c))
  skipSpace
  _ <- char '='
  skipMany $ notChar '\n'
  endOfLine <|> endOfInput
  return name
