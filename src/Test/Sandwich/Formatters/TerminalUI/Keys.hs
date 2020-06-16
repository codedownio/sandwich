-- |

module Test.Sandwich.Formatters.TerminalUI.Keys where

import qualified Data.List as L
import qualified Graphics.Vty as V

toggleShowRunTimesKey = V.KChar 't'
toggleShowContextManagersKey = V.KChar 'm'

cancelSelectedKey = V.KChar 'c'
cancelAllKey = V.KChar 'C'
clearResultsKey = V.KChar 'k'
runAgainKey = V.KChar 'r'

exitKey = V.KChar 'q'

toggleKeys = [V.KEnter, V.KChar '\t']

showKey (V.KChar '\t') = "Tab"
showKey (V.KChar c) = [c]
showKey V.KEnter = "Enter"

showKeys = L.intercalate "/" . fmap showKey
