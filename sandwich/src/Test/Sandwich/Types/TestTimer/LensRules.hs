
module Test.Sandwich.Types.TestTimer.LensRules (
  testTimerLensRules
  ) where

import Data.Char (toLower, toUpper)
import Language.Haskell.TH
import Lens.Micro
import Lens.Micro.TH


testTimerLensRules :: LensRules
testTimerLensRules = camelCaseFields
  & lensField .~ underscoreNoPrefixNamer

underscoreNoPrefixNamer :: Name -> [Name] -> Name -> [DefName]
underscoreNoPrefixNamer _ _ n =
  case nameBase n of
    '_':x:xs -> [MethodName (mkName ("Has" ++ (toUpper x:xs))) (mkName (toLower x:xs))]
    _        -> []
