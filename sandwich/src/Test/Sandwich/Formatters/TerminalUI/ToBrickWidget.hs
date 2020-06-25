{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.ToBrickWidget where

import Brick
import Brick.Widgets.Border
import qualified Data.List as L
import Data.String.Interpolate.IsString
import Data.Time.Clock
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Show.Pretty as P

class ToBrickWidget a where
  toBrickWidget :: a -> Widget n

instance ToBrickWidget Status where
  toBrickWidget (NotStarted {}) = strWrap "Not started"
  toBrickWidget (Running startTime) = strWrap [i|Started at #{startTime}|]
  toBrickWidget (Done startTime endTime Success) = strWrap [i|Succeeded in #{formatNominalDiffTime (diffUTCTime endTime startTime)}|]
  toBrickWidget (Done {statusResult=(Failure failureReason)}) = toBrickWidget failureReason

instance ToBrickWidget FailureReason where
  toBrickWidget (ExpectedButGot _ s1 s2) = hBox [
    hLimitPercent 50 (vBox [withAttr expectedAttr $ padBottom (Pad 1) $ str "Expected:", str s1])
    , padLeft (Pad 4) $ hLimitPercent 50 (vBox [withAttr gotAttr $ padBottom (Pad 1) $ str "Got:", str s2])
    ]
  toBrickWidget (ExpectedButGotValue _ v1 v2) = hBox [
    hLimitPercent 50 (vBox [withAttr expectedAttr $ padBottom (Pad 1) $ str "Expected:", toBrickWidget v1])
    , padLeft (Pad 4) $ hLimitPercent 50 (vBox [withAttr gotAttr $ padBottom (Pad 1) $ str "Got:", toBrickWidget v2])
    ]
  toBrickWidget (DidNotExpectButGot _ s) = hBox [
    vBox [str "Did not expect", str s]
    ]
  toBrickWidget (DidNotExpectButGotValue _ v) = hBox [
    vBox [str "Did not expect", toBrickWidget v]
    ]
  toBrickWidget x = strWrap [i|TODO: #{x}|]



instance ToBrickWidget P.Value where
  toBrickWidget (Integer s) = withAttr integerAttr $ str s
  toBrickWidget (Float s) = withAttr floatAttr $ str s
  toBrickWidget (Char s) = withAttr charAttr $ str s
  toBrickWidget (String s) = withAttr stringAttr $ str s
  toBrickWidget (Date s) = withAttr dateAttr $ str s
  toBrickWidget (Time s) = withAttr timeAttr $ str s
  toBrickWidget (Quote s) = withAttr quoteAttr $ str s
  toBrickWidget (Ratio v1 v2) = hBox [toBrickWidget v1, withAttr slashAttr $ str "/", toBrickWidget v2]
  toBrickWidget (Neg v) = hBox [withAttr negAttr $ str "-"
                               , toBrickWidget v]
  toBrickWidget (List vs) = vBox ((withAttr listBracketAttr $ str "[")
                                  : (fmap (padLeft (Pad 4)) listRows)
                                  <> [withAttr listBracketAttr $ str "]"])
    where listRows
            | length vs < 10 = fmap toBrickWidget vs
            | otherwise = (fmap toBrickWidget (L.take 3 vs))
                          <> [withAttr ellipsesAttr $ str "..."]
                          <> (fmap toBrickWidget (takeEnd 3 vs))
  toBrickWidget (Tuple vs) = vBox ((withAttr tupleBracketAttr $ str "(")
                                   : (fmap (padLeft (Pad 4)) tupleRows)
                                   <> [withAttr tupleBracketAttr $ str ")"])
    where tupleRows
            | length vs < 10 = fmap toBrickWidget vs
            | otherwise = (fmap toBrickWidget (L.take 3 vs))
                          <> [withAttr ellipsesAttr $ str "..."]
                          <> (fmap toBrickWidget (takeEnd 3 vs))
  toBrickWidget (Rec recordName tuples) = vBox (hBox [withAttr recordNameAttr $ str recordName, withAttr braceAttr $ str " {"]
                                                 : (fmap (padLeft (Pad 4)) recordRows)
                                                 <> [withAttr braceAttr $ str "}"])
    where recordRows
            | length tuples < 10 = fmap tupleToWidget tuples
            | otherwise = (fmap tupleToWidget (L.take 3 tuples))
                          <> [withAttr ellipsesAttr $ str "..."]
                          <> (fmap tupleToWidget (takeEnd 3 tuples))

          tupleToWidget (name, v) = hBox [withAttr fieldNameAttr $ str name
                                         , str " = "
                                         , toBrickWidget v]
  toBrickWidget (Con conName vs) = vBox ((withAttr constructorNameAttr $ str conName)
                                          : (fmap (padLeft (Pad 4)) constructorRows))
    where constructorRows
            | length vs < 10 = fmap toBrickWidget vs
            | otherwise = (fmap toBrickWidget (L.take 3 vs))
                          <> [withAttr ellipsesAttr $ str "..."]
                          <> (fmap toBrickWidget (takeEnd 3 vs))

  toBrickWidget (InfixCons opValue tuples) = vBox (L.intercalate [toBrickWidget opValue] [[x] | x <- rows])
    where rows
            | length tuples < 10 = fmap tupleToWidget tuples
            | otherwise = (fmap tupleToWidget (L.take 3 tuples))
                          <> [withAttr ellipsesAttr $ str "..."]
                          <> (fmap tupleToWidget (takeEnd 3 tuples))

          tupleToWidget (name, v) = hBox [withAttr fieldNameAttr $ str name
                                         , str " = "
                                         , toBrickWidget v]

-- * Util

takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
  where f (_:xs) (_:ys) = f xs ys
        f xs _ = xs
