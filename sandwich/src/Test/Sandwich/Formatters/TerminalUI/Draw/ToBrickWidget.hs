{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Sandwich.Formatters.TerminalUI.Draw.ToBrickWidget where

import Brick
import Brick.Widgets.Border
import Control.Exception.Safe
import Control.Monad.Reader
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Stack
import Safe
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Show.Pretty as P


class ToBrickWidget a where
  toBrickWidget :: a -> Reader CustomExceptionFormatters (Widget n)

instance ToBrickWidget Status where
  toBrickWidget (NotStarted {}) = return $ strWrap "Not started."
  toBrickWidget (Running {statusStartTime}) = return $ strWrap [i|Started at #{statusStartTime}.|]
  toBrickWidget (Done startTime endTime setupTime teardownTime Success) = return $ strWrap ([i|Succeeded in #{showTimeDiff startTime endTime}.#{setupTeardownInfo}|])
    where
      setupInfo :: Maybe T.Text = (\t -> [i|Setup: #{formatNominalDiffTime t}.|]) <$> setupTime
      teardownInfo :: Maybe T.Text = (\t -> [i|Teardown: #{formatNominalDiffTime t}.|]) <$> teardownTime
      setupTeardownInfo = case catMaybes [setupInfo, teardownInfo] of
        [] -> ""
        xs -> " (" <> T.intercalate " " xs <> ")"
  toBrickWidget (Done {statusResult=(Failure failureReason)}) = toBrickWidget failureReason
  toBrickWidget (Done {statusResult=DryRun}) = return $ strWrap "Not started due to dry run."
  toBrickWidget (Done {statusResult=Cancelled}) = return $ strWrap "Cancelled."

showTimeDiff :: UTCTime -> UTCTime -> String
showTimeDiff startTime endTime = formatNominalDiffTime (diffUTCTime endTime startTime)

instance ToBrickWidget FailureReason where
  toBrickWidget (ExpectedButGot _ (SEB x1) (SEB x2)) = do
    (widget1, widget2) <- case (P.reify x1, P.reify x2) of
      (Just v1, Just v2) -> (, ) <$> toBrickWidget v1 <*> toBrickWidget v2
      _ -> return (str (show x1), str (show x2))

    return $ hBox [
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
  toBrickWidget (DidNotExpectButGot _ x) = boxWithTitle "Did not expect:" <$> (reifyWidget x)
  toBrickWidget (Pending _ maybeMessage) = return $ case maybeMessage of
    Nothing -> withAttr pendingAttr $ str "Pending"
    Just msg -> hBox [withAttr pendingAttr $ str "Pending"
                     , str (": " <> msg)]
  toBrickWidget (Reason _ msg) = return $ boxWithTitle "Failure reason:" (strWrap msg)
  toBrickWidget (RawImage _ _ image) = return $ boxWithTitle "Failure reason:" (raw image)
  toBrickWidget (ChildrenFailed _ n) = return $ boxWithTitle [i|Reason: #{n} #{if n == 1 then ("child" :: String) else "children"} failed|] (strWrap "")
  toBrickWidget (GotException _ maybeMessage e@(SomeExceptionWithEq baseException)) = case fromException baseException of
    Just (fr :: FailureReason) -> boxWithTitle heading <$> (toBrickWidget fr)
    Nothing -> do
      customExceptionFormatters <- ask
      case headMay $ catMaybes [x baseException | x <- customExceptionFormatters] of
        Just (CustomTUIExceptionMessageAndCallStack msg _) -> return $ strWrap $ T.unpack msg
        Just (CustomTUIExceptionBrick widget) -> return $ boxWithTitle heading widget
        Nothing -> boxWithTitle heading <$> (reifyWidget e)
    where heading = case maybeMessage of
            Nothing -> "Got exception: "
            Just msg -> [i|Got exception (#{msg}):|]
  toBrickWidget (GotAsyncException _ maybeMessage e) = boxWithTitle heading <$> (reifyWidget e)
    where heading = case maybeMessage of
            Nothing -> "Got async exception: "
            Just msg -> [i|Got async exception (#{msg}):|]
  toBrickWidget (GetContextException _ e@(SomeExceptionWithEq baseException)) = case fromException baseException of
    Just (fr :: FailureReason) -> boxWithTitle "Get context exception:" <$> (toBrickWidget fr)
    _ -> boxWithTitle "Get context exception:" <$> (reifyWidget e)


boxWithTitle :: String -> Widget n -> Widget n
boxWithTitle heading inside = hBox [
  border $
    padAll 1 $
      (padBottom (Pad 1) (withAttr expectedAttr $ strWrap heading))
      <=>
      inside
  ]

reifyWidget :: Show a => a -> Reader CustomExceptionFormatters (Widget n)
reifyWidget x = case P.reify x of
  Just v -> toBrickWidget v
  _ -> return $ strWrap (show x)

instance ToBrickWidget P.Value where
  toBrickWidget (Integer s) = return $ withAttr integerAttr $ strWrap s
  toBrickWidget (Float s) = return $ withAttr floatAttr $ strWrap s
  toBrickWidget (Char s) = return $ withAttr charAttr $ strWrap s
  toBrickWidget (String s) = return $ withAttr stringAttr $ strWrap s
#if MIN_VERSION_pretty_show(1,10,0)
  toBrickWidget (Date s) = return $ withAttr dateAttr $ strWrap s
  toBrickWidget (Time s) = return $ withAttr timeAttr $ strWrap s
  toBrickWidget (Quote s) = return $ withAttr quoteAttr $ strWrap s
#endif
  toBrickWidget (Ratio v1 v2) = do
    w1 <- toBrickWidget v1
    w2 <- toBrickWidget v2
    return $ hBox [w1, withAttr slashAttr $ str "/", w2]
  toBrickWidget (Neg v) = do
    w <- toBrickWidget v
    return $ hBox [withAttr negAttr $ str "-", w]
  toBrickWidget (List vs) = do
    listRows <- abbreviateList vs
    return $ vBox ((withAttr listBracketAttr $ str "[")
                   : (fmap (padLeft (Pad 4)) listRows)
                   <> [withAttr listBracketAttr $ str "]"])
  toBrickWidget (Tuple vs) = do
    tupleRows <- abbreviateList vs
    return $ vBox ((withAttr tupleBracketAttr $ str "(")
                   : (fmap (padLeft (Pad 4)) tupleRows)
                   <> [withAttr tupleBracketAttr $ str ")"])
  toBrickWidget (Rec recordName tuples) = do
    recordRows <- abbreviateList' tupleToWidget tuples
    return $ vBox (hBox [withAttr recordNameAttr $ str recordName, withAttr braceAttr $ str " {"]
                        : (fmap (padLeft (Pad 4)) recordRows)
                        <> [withAttr braceAttr $ str "}"])
    where
      tupleToWidget (name, v) = toBrickWidget v >>= \w -> return $ hBox [
        withAttr fieldNameAttr $ str name
        , str " = "
        , w
        ]
  toBrickWidget (Con conName vs) = do
   constructorRows <- abbreviateList vs
   return $ vBox ((withAttr constructorNameAttr $ str conName)
                  : (fmap (padLeft (Pad 4)) constructorRows))
  toBrickWidget (InfixCons opValue tuples) = do
    rows <- abbreviateList' tupleToWidget tuples
    op <- toBrickWidget opValue
    return $ vBox (L.intercalate [op] [[x] | x <- rows])
    where
      tupleToWidget (name, v) = toBrickWidget v >>= \w -> return $ hBox [
        withAttr fieldNameAttr $ str name
        , str " = "
        , w
        ]

abbreviateList :: [Value] -> Reader CustomExceptionFormatters [Widget n]
abbreviateList = abbreviateList' toBrickWidget

abbreviateList' :: (Monad m) => (a -> m (Widget n)) -> [a] -> m [Widget n]
abbreviateList' f vs | length vs < 10 = mapM f vs
abbreviateList' f vs = do
  initial <- mapM f (L.take 3 vs)
  final <- mapM f (takeEnd 3 vs)
  return $ initial <> [withAttr ellipsesAttr $ str "..."] <> final

instance ToBrickWidget CallStack where
  toBrickWidget cs = vBox <$> (mapM renderLine $ getCallStack cs)
    where
      renderLine (f, srcLoc) = toBrickWidget srcLoc >>= \w -> return $ hBox [
        withAttr logFunctionAttr $ str f
        , str " called at "
        , w
        ]

instance ToBrickWidget SrcLoc where
  toBrickWidget (SrcLoc {..}) = return $ hBox [
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
