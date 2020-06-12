{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.Reader
import Control.Scheduler
import Data.Function
import Data.IORef
import Test.Sandwich
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Interpreters.RunTreeScheduler
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Spec
import UI.NCurses

import qualified Test.Sandwich.Interpreters.NCursesTest as N


topSpec :: TopSpec
topSpec = do
  before "asdf" (\() -> putStrLn "Before") $ do
    it "does the first thing" pending
    it "does the second thing" pending
    it "does the third thing" pending
    describe "nested stuff" $ do
      it "does a nested thing" pending

  around "some around" (\action -> action (() :> ())) $ do
    it "does 1" pending
    it "does 2" pending
  
  introduce "Intro a string" (\() -> getLine >>= \s -> return (s :> ())) (\_ -> return ()) $ do
    it "uses the string" $ \(str :> ()) -> do
      putStrLn $ "Got the string: " <> str
      return Success

    it "uses the string again" $ \(str :> ()) -> do
      putStrLn $ "Got the string here: " <> str
      return Success

  it "does a thing" $ \() -> do
    putStrLn "HI"
    return Success

  describe "it does this thing also" $ do
    it "does a sub-test" pending

  describeParallel "it does this thing also" $ do
    it "does a first sub-test 1" pending
    it "does a sub-test 2" pending
    it "does a sub-test 3" pending


mainFilter :: IO ()
mainFilter = putStrLn $ prettyShow $ filterTree "also" topSpec

main :: IO ()
main = do
  withScheduler_ (ParN 2) $ \sched -> do
    asyncUnit <- async (return ())
    rts <- runReaderT (runTree topSpec) (asyncUnit, sched)

    print rts

    runCurses $ do
      setEcho False
      w <- defaultWindow

      colorMap <- ColorMap
        <$> newColorID ColorGreen ColorDefault 1
        <*> newColorID ColorRed ColorDefault 2
        <*> newColorID ColorCyan ColorDefault 3
        <*> newColorID ColorYellow ColorDefault 4
        <*> newColorID ColorBlue ColorDefault 5
        <*> newColorID ColorDefault ColorDefault 6

      updateAll w colorMap rts

      fix $ \loop -> do
        getEvent w (Just 100) >>= \case
          Nothing -> do
            updateAll w colorMap rts
            loop
          Just ev' -> if ev' == EventCharacter 'q' then return () else loop

updateAll w colorMap rts = do
  rtFixed <- liftIO $ forM rts fixRunTree
  updateWindow w $ do
    drawBox Nothing Nothing
    drawRunTree colorMap rtFixed
  render


data ColorMap = ColorMap {
  colorGreen :: ColorID
  , colorRed :: ColorID
  , colorOrange :: ColorID
  , colorYellow :: ColorID
  , colorBlue :: ColorID
  , colorDefault :: ColorID
  }

drawRunTree :: ColorMap -> [RunTreeFixed] -> Update ()
drawRunTree colorMap rt = do
  runRWST (drawRunTree' rt) colorMap (2, 2)
  return ()

type DrawM = RWST ColorMap [()] (Integer, Integer) Update

drawRunTree' :: [RunTreeFixed] -> DrawM ()
drawRunTree' rts = do
  (line, ch) <- get

  forM_ rts $ \rt -> do
    drawRunTree'' rt
    advanceLine

drawRunTree'' :: RunTreeFixed -> DrawM ()
drawRunTree'' (RunTreeSingle {..}) = do
  setPosition
  setColorByStatus runTreeStatus
  lift $ drawString runTreeLabel
drawRunTree'' (RunTreeGroup {..}) = do
  setPosition
  setColorByStatus runTreeStatus
  lift $ drawString runTreeLabel
  advanceLine
  advanceColumn
  drawRunTree' runTreeChildren
  retreatColumn

indent = 4

setColorByStatus status = do
  ColorMap {..} <- RWS.ask

  let color = case status of
        NotStarted -> colorDefault
        Running {} -> colorYellow
        Done Success -> colorGreen
        Done (Pending {}) -> colorOrange
        Done (Failure {}) -> colorRed

  lift $ setColor color

advanceLine = do
  modify $ \(line, ch) -> (line + 1, ch)
  setPosition
advanceColumn = do
  modify $ \(line, ch) -> (line, ch + indent)
  setPosition
retreatColumn = do
  modify $ \(line, ch) -> (line, ch - indent)
  setPosition

setPosition = do
  (line, ch) <- get
  lift $ moveCursor line ch
