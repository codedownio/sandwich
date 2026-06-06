{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.TerminalUI.Draw.Util where

import Brick
import Graphics.Vty.Image
import Lens.Micro


fixedHeightOrViewportPercent :: (Ord n, Show n) => n -> Int -> Widget n -> Widget n
fixedHeightOrViewportPercent vpName maxHeightPercent w =
  Widget Fixed Fixed $ do
    -- Render the viewport contents in advance
    result <- render w
    -- If the contents will fit in the maximum allowed rows,
    -- just return the content without putting it in a viewport.

    ctx <- getContext

#if MIN_VERSION_brick(0,56,0)
    let usableHeight = ctx ^. windowHeightL
#else
    let usableHeight = min 80 (ctx ^. availHeightL) -- Bound this so it looks okay inside a viewport
#endif

    let maxHeight = round (toRational usableHeight * (toRational maxHeightPercent / 100))

    if imageHeight (image result) <= maxHeight
      then return result
      -- Otherwise put the contents in a viewport and limit the height to the
      -- maximum allowable height. Show a scroll bar on the right so it's clear
      -- the content is scrollable. Re-render the content slightly narrower than
      -- the available width so its right border is drawn just left of the
      -- scroll bar, which occupies the viewport's rightmost column, rather than
      -- underneath it.
      else render (vLimit maxHeight $
                   withVScrollBars OnRight $
                   viewport vpName Vertical $
                   hLimit (ctx ^. availWidthL - 2) w)
