-- |

module Test.Sandwich.Formatters.Print.Util where

import qualified Data.List as L
import Text.Show.Pretty as P


isSingleLine :: P.Value -> Bool
isSingleLine (Con {}) = False
isSingleLine (InfixCons op tuples) = isSingleLine op && (all isSingleLine (fmap snd tuples))
isSingleLine (Rec {}) = False
isSingleLine (Tuple values) = all isSingleLine values
isSingleLine (List values) = all isSingleLine values
isSingleLine (Neg value) = isSingleLine value
isSingleLine (Ratio v1 v2) = all isSingleLine [v1, v2]
isSingleLine (String s) = '\n' `L.notElem` s
isSingleLine (Quote s) = '\n' `L.notElem` s
isSingleLine _ = True
