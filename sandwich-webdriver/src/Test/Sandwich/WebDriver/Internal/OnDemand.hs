
module Test.Sandwich.WebDriver.Internal.OnDemand where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.MVar


getOnDemand :: forall m a. (
  MonadUnliftIO m, MonadLogger m
  ) => MVar (OnDemand a) -> m (Either Text a) -> m a
getOnDemand onDemandVar doObtain = do
  result <- modifyMVar onDemandVar $ \case
    OnDemandErrored msg -> expectationFailure (T.unpack msg)
    OnDemandNotStarted -> do
      asy <- async $ do
        let handler :: SomeException -> m a
            handler e = do
              modifyMVar_ onDemandVar (const $ return $ OnDemandErrored [i|Got exception: #{e}|])
              throwIO e

        handle handler $ do
          doObtain >>= \case
            Left err -> do
              modifyMVar_ onDemandVar (const $ return $ OnDemandErrored err)
              expectationFailure [i|Failed to obtain: #{err}|]

            Right x -> do
              modifyMVar_ onDemandVar (const $ return $ OnDemandReady x)
              return x

      return (OnDemandInProgress asy, Left asy)

    od@(OnDemandInProgress asy) -> pure (od, Left asy)
    od@(OnDemandReady x) -> pure (od, Right x)

  case result of
    Right x -> pure x
    Left asy -> wait asy
