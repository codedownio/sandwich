{-# LANGUAGE QuasiQuotes #-}

{-|

Render a simple, dependency-free SVG line chart of per-pod time series. Used by
the resource watcher (see "Test.Sandwich.Contexts.Kubernetes.ResourceWatcher") to
plot per-pod CPU and memory over time.

-}

module Test.Sandwich.Contexts.Kubernetes.Util.SeriesPlot (
  renderSeriesSvg
  ) where

import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import Relude


-- | Render an SVG line chart. The input maps each series name (e.g. a pod) to a
-- time series of @(elapsedSeconds, value)@ samples, where @value@ is already in
-- the desired display units. One colored polyline is drawn per series, with axes
-- and a legend. Returns the SVG document as 'Text'.
renderSeriesSvg ::
  -- | Chart title (include the unit, e.g. @"Pod memory usage over time (MiB)"@)
  Text
  -- | Message to show when no samples were collected
  -> Text
  -- | @seriesName -> [(elapsedSeconds, value)]@
  -> Map Text [(Double, Double)]
  -> Text
renderSeriesSvg title emptyMessage samples
  | M.null nonEmptySeries = emptySvg
  | otherwise = svg
  where
    nonEmptySeries = M.filter (not . null) samples

    series :: [(Text, [(Double, Double)])] -- (name, [(t, value)])
    series = [ (name, sortOn fst pts) | (name, pts) <- M.toList nonEmptySeries ]

    allTs = [t | (_, pts) <- series, (t, _) <- pts]
    allMs = [m | (_, pts) <- series, (_, m) <- pts]
    maxT = let x = foldl' max 0 allTs in if x <= 0 then 1 else x
    maxM = let x = foldl' max 0 allMs in if x <= 0 then 1 else x

    -- Layout
    width, height, left, right, top, bottom :: Double
    width = 960; height = 540
    left = 70; right = 230; top = 30; bottom = 50
    plotW = width - left - right
    plotH = height - top - bottom

    sx t = left + (t / maxT) * plotW
    sy m = top + plotH - (m / maxM) * plotH

    px :: Double -> Text
    px v = show (round v :: Int)

    palette :: [Text]
    palette = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
              , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"]
    colorFor n = fromMaybe "#000000" (palette !!? (n `mod` length palette))

    -- Horizontal gridlines + y labels
    gridDivs = 4 :: Int
    gridlines = T.concat [ let frac = fromIntegral k / fromIntegral gridDivs
                               y = top + plotH - frac * plotH
                               label = round (frac * maxM) :: Int
                           in [i|<line x1="#{px left}" y1="#{px y}" x2="#{px (left + plotW)}" y2="#{px y}" stroke="\#eee" />
<text x="#{px (left - 8)}" y="#{px (y + 4)}" text-anchor="end" font-size="11" fill="\#555">#{label}</text>
|]
                         | k <- [0 .. gridDivs] ]

    -- X axis ticks at 0, mid, max
    xticks = T.concat [ let x = sx t
                        in [i|<text x="#{px x}" y="#{px (top + plotH + 18)}" text-anchor="middle" font-size="11" fill="\#555">#{round t :: Int}s</text>
|]
                      | t <- [0, maxT / 2, maxT] ]

    polylines = T.concat [ let pts = T.intercalate " " [[i|#{px (sx t)},#{px (sy m)}|] | (t, m) <- ser]
                           in [i|<polyline fill="none" stroke="#{colorFor idx}" stroke-width="1.5" points="#{pts}" />
|]
                         | (idx, (_name, ser)) <- zip [0 ..] series ]

    legend = T.concat [ let y = top + 6 + fromIntegral idx * 18
                            nm = T.take 38 name
                        in [i|<rect x="#{px (left + plotW + 16)}" y="#{px (y - 9)}" width="11" height="11" fill="#{colorFor idx}" />
<text x="#{px (left + plotW + 32)}" y="#{px y}" font-size="11" fill="\#333">#{nm}</text>
|]
                      | (idx, (name, _)) <- zip [0 :: Int ..] series ]

    svg = [i|<svg xmlns="http://www.w3.org/2000/svg" width="#{px width}" height="#{px height}" font-family="sans-serif">
<rect width="100%" height="100%" fill="white" />
<text x="#{px left}" y="18" font-size="14" fill="\#222">#{title}</text>
#{gridlines}<line x1="#{px left}" y1="#{px top}" x2="#{px left}" y2="#{px (top + plotH)}" stroke="\#999" />
<line x1="#{px left}" y1="#{px (top + plotH)}" x2="#{px (left + plotW)}" y2="#{px (top + plotH)}" stroke="\#999" />
#{xticks}#{polylines}#{legend}</svg>
|]

    emptySvg = [i|<svg xmlns="http://www.w3.org/2000/svg" width="480" height="80" font-family="sans-serif">
<rect width="100%" height="100%" fill="white" />
<text x="12" y="44" font-size="14" fill="\#555">#{emptyMessage}</text>
</svg>
|]
