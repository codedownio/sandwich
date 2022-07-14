{-# LANGUAGE CPP #-}

module Test.Sandwich.Hedgehog.Render (renderHedgehogToImage) where

import Graphics.Vty.Attributes
import Graphics.Vty.Image
import Hedgehog.Internal.Report
import Text.PrettyPrint.Annotated.WL (Doc)
import qualified Text.PrettyPrint.Annotated.WL as WL


data Token = Str String
           | NewAttr Attr
  deriving (Show)

  -- None
  -- | RedDull | RedBoldVivid | RedVivid
  -- | Yellow | YellowDull
  -- | Green | GreenDull
  -- | Black | BlackVivid
  -- | MagentaDull
  -- | Str String

defaultAttr = Attr Default Default Default Default
redVivid = withForeColor defaultAttr brightRed
redDull = withForeColor defaultAttr red
redVividBold = flip withStyle bold $ withForeColor defaultAttr brightRed
yellowDull = withForeColor defaultAttr yellow
magentaDull = withForeColor defaultAttr magenta
greenDull = withForeColor defaultAttr green
blackVivid = withForeColor defaultAttr brightBlack

renderHedgehogToImage :: Doc Markup -> Image
renderHedgehogToImage doc = foldTokens defaultAttr (mconcat $ renderHedgehogToColors doc)

foldTokens currentAttr ((Str "\n"):xs) = foldTokens currentAttr xs
foldTokens currentAttr ((Str s):xs) = string currentAttr s <-> foldTokens currentAttr xs
foldTokens _currentAttr ((NewAttr attr):xs) = foldTokens attr xs
foldTokens _currentAttr [] = emptyImage

renderHedgehogToColors :: Doc Markup -> [[Token]]
renderHedgehogToColors doc =
  pure .
    WL.displayDecorated (\x -> [NewAttr $ start x]) end (\x -> [Str x]) .
    WL.renderSmart 100 $
    WL.indent 0 doc

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
