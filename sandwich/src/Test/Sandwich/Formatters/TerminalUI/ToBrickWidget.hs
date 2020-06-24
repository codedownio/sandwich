{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.ToBrickWidget where

import Brick
import Brick.Widgets.Border
import Data.String.Interpolate.IsString
import Data.Time.Clock
import Test.Sandwich.Formatters.TerminalUI.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Show.Pretty

class ToBrickWidget a where
  toBrickWidget :: a -> Widget n

instance ToBrickWidget Status where
  toBrickWidget (NotStarted {}) = strWrap "Not started"
  toBrickWidget (Running startTime) = strWrap [i|Started at #{startTime}|]
  toBrickWidget (Done startTime endTime Success) = strWrap [i|Succeeded in #{formatNominalDiffTime (diffUTCTime endTime startTime)}|]
  toBrickWidget (Done {statusResult=(Failure failureReason)}) = toBrickWidget failureReason

instance ToBrickWidget FailureReason where
  toBrickWidget (ExpectedButGot _ s1 s2) = hBox [
    hLimitPercent 50 (vBox [str "Expected", str s1])
    , hLimitPercent 50 (vBox [str "Got", str s2])
    ]
  toBrickWidget (ExpectedButGotValue _ v1 v2) = hBox [
    hLimitPercent 50 (vBox [str "Expected", str $ show v1])
    , hLimitPercent 50 (vBox [str "Got", str $ show v2])
    ]
  toBrickWidget x = strWrap [i|TODO: #{x}|]
