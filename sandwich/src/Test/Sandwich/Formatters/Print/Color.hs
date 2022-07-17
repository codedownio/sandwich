
module Test.Sandwich.Formatters.Print.Color where

import Data.Colour.RGBSpace
import Data.Colour.SRGB

expectedColor = midWhite
sawColor = midWhite
integerColor = solarizedMagenta
floatColor = solarizedMagenta
charColor = solarizedCyan
stringColor = solarizedYellow
dateColor = solarizedBase2
timeColor = solarizedBase3
quoteColor = solarizedBase1
slashColor = solarizedViolet
negColor = solarizedViolet
listBracketColor = solarizedOrange -- TODO: make green?
tupleBracketColor = solarizedGreen
braceColor = solarizedGreen
ellipsesColor = solarizedBase0
recordNameColor = solarizedRed
fieldNameColor = solarizedYellow
constructorNameColor = solarizedViolet


midWhite = sRGB24 c c c :: Colour Float
  where c = 0xc0

midGray = sRGB24 c c c :: Colour Float
  where c = 0x70

solarizedBase03 = sRGB24 0x00 0x2b 0x36 :: Colour Float
solarizedBase02 = sRGB24 0x07 0x36 0x42 :: Colour Float
solarizedBase01 = sRGB24 0x58 0x6e 0x75 :: Colour Float
solarizedbase00 = sRGB24 0x65 0x7b 0x83 :: Colour Float
solarizedBase0 = sRGB24 0x83 0x94 0x96 :: Colour Float
solarizedBase1 = sRGB24 0x93 0xa1 0xa1 :: Colour Float
solarizedBase2 = sRGB24 0xee 0xe8 0xd5 :: Colour Float
solarizedBase3 = sRGB24 0xfd 0xf6 0xe3 :: Colour Float
solarizedYellow = sRGB24 0xb5 0x89 0x00 :: Colour Float
solarizedOrange = sRGB24 0xcb 0x4b 0x16 :: Colour Float
solarizedRed = sRGB24 0xdc 0x32 0x2f :: Colour Float
solarizedMagenta = sRGB24 0xd3 0x36 0x82 :: Colour Float
solarizedViolet = sRGB24 0x6c 0x71 0xc4 :: Colour Float
solarizedBlue = sRGB24 0x26 0x8b 0xd2 :: Colour Float
solarizedCyan = sRGB24 0x2a 0xa1 0x98 :: Colour Float
solarizedGreen = sRGB24 0x85 0x99 0x00 :: Colour Float
