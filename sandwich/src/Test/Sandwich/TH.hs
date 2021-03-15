{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Sandwich.TH (
  getSpecFromFolder
  ) where

import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath as F
import Test.Sandwich.Types.Spec hiding (location)


constId = const id

getSpecFromFolder :: Name -> Q Exp
getSpecFromFolder = getSpecFromFolder' 'constId

getSpecFromFolder' :: Name -> Name -> Q Exp
getSpecFromFolder' individualSpecHooks combiner = do
  dir <- runIO getCurrentDirectory
  filename <- loc_filename <$> location
  let folder = dropExtension (dir </> filename)
  getSpecFromFolder'' folder individualSpecHooks combiner

getSpecFromFolder'' :: F.FilePath -> Name -> Name -> Q Exp
getSpecFromFolder'' folder individualSpecHooks combiner = do
  items <- qRunIO $ L.sort <$> listDirectory folder
  specs <- (catMaybes <$>) $ forM items $ \item -> do
    isDirectory <- qRunIO $ doesDirectoryExist (folder </> item)

    if | isDirectory -> do
           qRunIO (doesFileExist (folder </> item <.> "hs")) >>= \case
             False -> Just <$> getSpecFromFolder'' (folder </> item) individualSpecHooks combiner
             True -> return Nothing -- Do nothing, allow the .hs file to be picked up separately
       | takeExtension item == ".hs" ->
           Just <$> [e|$(varE 'alterTopLevelNodeOptions) (\x -> x { nodeOptionsMainFunction = Just ($(conE 'NodeMainFunction) "asdf" (return ())) })
                       $ $(varE individualSpecHooks) $(stringE item) $(varE $ mkName $ takeBaseName item ++ ".tests")|]
       | otherwise -> return Nothing

  [e|$(varE combiner) $(stringE $ mangleFolderName folder) (L.foldl1 (>>) $(listE $ fmap return specs))|]

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
