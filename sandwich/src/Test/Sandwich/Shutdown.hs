{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Shutdown where

import Control.Concurrent.Async
import Control.Monad
import Test.Sandwich.Types.RunTree


-- cancelRecursively :: RunTreeWithStatus s l t -> IO ()
-- cancelRecursively (RunTreeGroup {..}) = do
--   forM_ runTreeChildren cancelRecursively
--   cancel runTreeAsync
-- cancelRecursively (RunTreeSingle {..}) =
--   cancel runTreeAsync

cancelRecursively = undefined
