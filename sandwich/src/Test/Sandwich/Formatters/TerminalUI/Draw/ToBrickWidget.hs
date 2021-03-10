{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sandwich.Formatters.TerminalUI.Draw.ToBrickWidget where

import Brick
import Brick.Widgets.Border
import Control.Exception.Safe
import qualified Data.List as L
import Data.String.Interpolate
import Data.Time.Clock
import GHC.Stack
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Show.Pretty as P

class ToBrickWidget a where
  toBrickWidget :: a -> Widget n

instance ToBrickWidget Status where
  toBrickWidget (NotStarted {}) = strWrap "Not started"
  toBrickWidget (Running {statusStartTime}) = strWrap [i|Started at #{statusStartTime}|]
  toBrickWidget (Done startTime endTime Success) = strWrap [i|Succeeded in #{formatNominalDiffTime (diffUTCTime endTime startTime)}|]
  toBrickWidget (Done {statusResult=(Failure failureReason)}) = toBrickWidget failureReason

instance ToBrickWidget FailureReason where
  toBrickWidget (ExpectedButGot _ (SEB x1) (SEB x2)) = hBox [
    hLimitPercent 50 $
      border $
        padAll 1 $
          (padBottom (Pad 1) (withAttr expectedAttr $ str "Expected:"))
          <=>
          widget1
    , padLeft (Pad 1) $
        hLimitPercent 50 $
          border $
            padAll 1 $
              (padBottom (Pad 1) (withAttr sawAttr $ str "Saw:"))
              <=>
              widget2
    ]
    where
      (widget1, widget2) = case (P.reify x1, P.reify x2) of
        (Just v1, Just v2) -> (toBrickWidget v1, toBrickWidget v2)
        _ -> (str (show x1), str (show x2))
  toBrickWidget (DidNotExpectButGot _ x) = boxWithTitle "Did not expect:" (reifyWidget x)
  toBrickWidget (Pending _ maybeMessage) = case maybeMessage of
    Nothing -> withAttr pendingAttr $ str "Pending"
    Just msg -> hBox [withAttr pendingAttr $ str "Pending"
                     , str (": " <> msg)]
  toBrickWidget (Reason _ msg) = boxWithTitle "Failure reason:" (strWrap msg)
  toBrickWidget (GotException _ maybeMessage e@(SomeExceptionWithEq baseException)) = case fromException baseException of
    Just (fr :: FailureReason) -> boxWithTitle heading (toBrickWidget fr)
    _ -> boxWithTitle heading (reifyWidget e)
    where heading = case maybeMessage of
            Nothing -> "Got exception: "
            Just msg -> [i|Got exception (#{msg}):|]
  toBrickWidget (GotAsyncException _ maybeMessage e) = boxWithTitle heading (reifyWidget e)
    where heading = case maybeMessage of
            Nothing -> "Got async exception: "
            Just msg -> [i|Got async exception (#{msg}):|]
  toBrickWidget (GetContextException _ e@(SomeExceptionWithEq baseException)) = case fromException baseException of
    Just (fr :: FailureReason) -> boxWithTitle "Get context exception:" (toBrickWidget fr)
    _ -> boxWithTitle "Get context exception:" (reifyWidget e)


boxWithTitle heading inside = hBox [
  border $
    padAll 1 $
      (padBottom (Pad 1) (withAttr expectedAttr $ strWrap heading))
      <=>
      inside
  ]

reifyWidget x = case P.reify x of
  Just v -> toBrickWidget v
  _ -> strWrap (show x)

instance ToBrickWidget P.Value where
  toBrickWidget (Integer s) = withAttr integerAttr $ strWrap s
  toBrickWidget (Float s) = withAttr floatAttr $ strWrap s
  toBrickWidget (Char s) = withAttr charAttr $ strWrap s
  toBrickWidget (String s) = withAttr stringAttr $ strWrap s
#if MIN_VERSION_pretty_show(1,10,0)
  toBrickWidget (Date s) = withAttr dateAttr $ strWrap s
  toBrickWidget (Time s) = withAttr timeAttr $ strWrap s
  toBrickWidget (Quote s) = withAttr quoteAttr $ strWrap s
#endif
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

instance ToBrickWidget CallStack where
  toBrickWidget cs = vBox (fmap renderLine $ getCallStack cs)
    where
      renderLine (f, srcLoc) = hBox [
        withAttr logFunctionAttr $ str f
        , str " called at "
        , toBrickWidget srcLoc
        ]

instance ToBrickWidget SrcLoc where
  toBrickWidget (SrcLoc {..}) = hBox [
    withAttr logFilenameAttr $ str srcLocFile
    , str ":"
    , withAttr logLineAttr $ str $ show srcLocStartLine
    , str ":"
    , withAttr logChAttr $ str $ show srcLocStartCol
    , str " in "
    , withAttr logPackageAttr $ str srcLocPackage
    , str ":"
    , str srcLocModule
    ]

-- * Util

takeEnd :: Int -> [a] -> [a]
takeEnd j xs = f xs (drop j xs)
  where f (_:zs) (_:ys) = f zs ys
        f zs _ = zs
