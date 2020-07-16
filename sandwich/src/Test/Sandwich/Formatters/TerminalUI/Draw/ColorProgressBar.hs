{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Draw.ColorProgressBar (
  bottomProgressBarColored
  ) where

import Brick
import Data.Foldable
import Data.Ord (comparing)
import Data.String.Interpolate
import GHC.Stack
import Lens.Micro
import Lens.Micro.TH
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree
import Test.Sandwich.RunTree
import Test.Sandwich.Types.Spec

type Chunk a = [(Rational, a)]

data ChunkSum = ChunkSum { _running :: Rational
                         , _notStarted :: Rational
                         , _pending :: Rational
                         , _success :: Rational
                         , _failure :: Rational }

zeroChunkSum :: ChunkSum
zeroChunkSum = ChunkSum 0 0 0 0 0

makeLenses ''ChunkSum

splitIntoChunks :: forall a. (Show a) => Rational -> [(Rational, a)] -> [[(Rational, a)]]
splitIntoChunks _ [] = []
splitIntoChunks chunkSize remaining = chunk : (splitIntoChunks chunkSize remaining')
  where
    (chunk, remaining') = go [] chunkSize remaining

    go :: Chunk a -> Rational -> [(Rational, a)] -> (Chunk a, [(Rational, a)])
    go chunkSoFar needed ((amount, val):xs) =
      if | amount == needed -> (chunkSoFar <> [(amount, val)], xs)
         | amount < needed -> go (chunkSoFar <> [(amount, val)]) (needed - amount) xs
         | amount > needed -> (chunkSoFar <> [(needed, val)], (amount - needed, val):xs)
    go chunkSoFar needed [] = error [i|Bottomed out in go: #{chunkSoFar}, #{needed}|]

-- TODO: improve this to use block chars
getCharForChunk :: [(Rational, Status)] -> Widget n
getCharForChunk chunk = withAttr attrToUse (str full_five_eighth_height)
  where ChunkSum {..} = sumChunk chunk
        (_, attrToUse) = maxBy fst [(_running, runningAttr)
                                   , (_notStarted, notStartedAttr)
                                   , (_pending, pendingAttr)
                                   , (_success, successAttr)
                                   , (_failure, failureAttr)
                                   ]

sumChunk :: Chunk Status -> ChunkSum
sumChunk = foldl combine zeroChunkSum
  where combine chunkSum (amount, status) = chunkSum & (lensForStatus status) %~ (+ amount)

        lensForStatus NotStarted = notStarted
        lensForStatus (Running {}) = running
        lensForStatus (Done {statusResult=Success}) = success
        lensForStatus (Done {statusResult=(Failure (Pending {}))}) = pending
        lensForStatus (Done {statusResult=(Failure _)}) = failure

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

-- * Block elems

full = "█"
seven_eighth = "▉"
six_eighth = "▊"
five_eighth = "▋"
four_eighth = "▌"
three_eighth = "▍"
two_eighth = "▎"
one_eighth = "▏"


full_five_eighth_height = "▆"

-- * Exports

bottomProgressBarColored app = Widget Greedy Fixed $ do
  c <- getContext
  render $ bottomProgressBarColoredWidth app (c ^. availWidthL)

bottomProgressBarColoredWidth app width = hBox [getCharForChunk chunk | chunk <- chunks]
  where
    statuses = concatMap getStatuses (app ^. appRunTreeFiltered)
    statusesWithAmounts = [(testsPerChar, x) | x <- statuses]

    chunks = splitIntoChunks 1 statusesWithAmounts

    testsPerChar :: Rational = fromIntegral width / fromIntegral (length statuses)

    getStatuses :: (HasCallStack) => RunNodeWithStatus context a l t -> [a]
    getStatuses = extractValues (runTreeStatus . runNodeCommon)
