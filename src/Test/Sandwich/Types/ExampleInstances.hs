{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Test.Sandwich.Types.ExampleInstances where

import Test.Sandwich.Types.Example
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

data Dummy = Dummy

instance Example context Dummy where
  evaluateExample dummy = undefined

instance Example context (context -> IO Result) where
  evaluateExample e = do
    ItemContext {..} <- ask
    liftIO $ e itemContextContext

instance Example context (context -> IO ()) where
  evaluateExample e = do
    ItemContext {..} <- ask
    liftIO $ e itemContextContext
    return $ Result "" Success
