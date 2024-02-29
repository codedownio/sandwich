{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Sandwich.Hedgehog.Render (
  renderHedgehogToImage
  , renderHedgehogToTokens

  -- * Util
  , dedent
  ) where

import Data.Function
import qualified Data.List as L
import qualified Data.Text as T
import Graphics.Vty.Attributes hiding (currentAttr)
import Graphics.Vty.Image
import Hedgehog.Internal.Report
import Text.PrettyPrint.Annotated.WL (Doc)
import qualified Text.PrettyPrint.Annotated.WL as WL


renderHedgehogToImage :: Doc Markup -> Image
renderHedgehogToImage doc = foldTokens emptyImage defaultAttr $ renderHedgehogToTokens doc

foldTokens imageSoFar currentAttr ((Str "\n"):xs) = (if imageSoFar == emptyImage then text defaultAttr " " else imageSoFar) <-> foldTokens emptyImage currentAttr xs
foldTokens imageSoFar currentAttr ((Str s):xs) = foldTokens (imageSoFar <|> text' currentAttr s) currentAttr xs
foldTokens imageSoFar _currentAttr ((NewAttr attr):xs) = foldTokens imageSoFar attr xs
foldTokens imageSoFar _currentAttr [] = imageSoFar

renderHedgehogToTokens :: Doc Markup -> [Token]
renderHedgehogToTokens doc =
  WL.indent 0 doc
  & WL.renderSmart 100
  & WL.displayDecorated (\x -> [NewAttr $ start x]) end (\x -> [Str (T.pack x)])
  & joinAdjacentStrings
  & splitNewlines
  where
    joinAdjacentStrings ((Str s1):(Str s2):xs) = joinAdjacentStrings (Str (s1 <> s2) : xs)
    joinAdjacentStrings (x:xs) = x : joinAdjacentStrings xs
    joinAdjacentStrings [] = []

    splitNewlines :: [Token] -> [Token]
    splitNewlines ((Str s):xs) = [Str s' | s' <- parts, s' /= ""] <> splitNewlines xs
      where parts = L.intersperse "\n" $ T.splitOn "\n" s
    splitNewlines (x:xs) = x : splitNewlines xs
    splitNewlines [] = []

data Token = Str T.Text
           | NewAttr Attr
  deriving (Show)

dedent :: Int -> String -> String
dedent n s
  | (replicate n ' ') `L.isPrefixOf` s = L.drop n s
  | otherwise = s

-- * This all is modeled after Hedgehog.Internal.Report

defaultAttr = Attr Default Default Default Default
redVivid = withForeColor defaultAttr brightRed
redDull = withForeColor defaultAttr red
redVividBold = flip withStyle bold $ withForeColor defaultAttr brightRed
yellowDull = withForeColor defaultAttr yellow
magentaDull = withForeColor defaultAttr magenta
greenDull = withForeColor defaultAttr green
blackVivid = withForeColor defaultAttr brightBlack

start = \case
  WaitingIcon -> defaultAttr
  WaitingHeader -> defaultAttr
  RunningIcon -> defaultAttr
  RunningHeader -> defaultAttr
  ShrinkingIcon -> redVivid
  ShrinkingHeader -> redVivid
  FailedIcon -> redVivid
  FailedText -> redVivid
  GaveUpIcon -> yellowDull
  GaveUpText -> yellowDull
  SuccessIcon -> greenDull
  SuccessText -> greenDull
  CoverageIcon -> yellowDull
  CoverageText -> yellowDull
  CoverageFill -> blackVivid

  DeclarationLocation -> defaultAttr

  StyledLineNo StyleDefault -> defaultAttr
  StyledSource StyleDefault -> defaultAttr
  StyledBorder StyleDefault -> defaultAttr

  StyledLineNo StyleAnnotation -> magentaDull
  StyledSource StyleAnnotation -> defaultAttr
  StyledBorder StyleAnnotation -> defaultAttr
  AnnotationGutter -> magentaDull
  AnnotationValue -> magentaDull

  StyledLineNo StyleFailure -> redVivid
  StyledSource StyleFailure -> redVividBold
  StyledBorder StyleFailure -> defaultAttr
  FailureArrows -> redVivid
  FailureMessage -> defaultAttr
  FailureGutter -> defaultAttr

  DiffPrefix -> defaultAttr
  DiffInfix -> defaultAttr
  DiffSuffix -> defaultAttr
  DiffSame -> defaultAttr
  DiffRemoved -> redDull
  DiffAdded -> greenDull

  ReproduceHeader -> defaultAttr
  ReproduceGutter -> defaultAttr
  ReproduceSource -> defaultAttr

end _ = [NewAttr defaultAttr]
