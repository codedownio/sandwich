
module Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform (
  detectPlatform
  , Platform(..)
  ) where

import Data.String.Interpolate
import qualified System.Info as SI


data Platform = Linux | OSX | Windows
  deriving (Show, Eq)

detectPlatform :: Platform
detectPlatform = case SI.os of
  "windows" -> Windows
  "linux" -> Linux
  "darwin" -> OSX
  _ -> error [i|Couldn't determine host platform from string: '#{SI.os}'|]
