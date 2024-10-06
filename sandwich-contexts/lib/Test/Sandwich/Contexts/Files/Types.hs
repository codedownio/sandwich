{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Files.Types (
  askFile
  , askFile'

  , EnvironmentFile(..)
  , HasFile
  , mkFileLabel
  ) where

import GHC.TypeLits
import Relude
import Test.Sandwich


-- | Retrieve a file context.
askFile :: forall a context m. (MonadReader context m, HasFile context a) => m FilePath
askFile = askFile' (Proxy @a)

-- | Variant of 'askFile' that you can use with a 'Proxy' rather than a type application.
askFile' :: forall a context m. (MonadReader context m, HasFile context a) => Proxy a -> m FilePath
askFile' _ = unEnvironmentFile <$> getContext (mkFileLabel @a)

-- | A file path to make available to tests.
-- For example, this can be an external binary like "minikube" if a given test context wants
-- to use it to start a Minikube cluster.
-- But you can use this for any kind of file you want to inject into tests.
data EnvironmentFile a = EnvironmentFile { unEnvironmentFile :: FilePath }

-- | Has-* class for asserting a given file is available.
type HasFile context a = HasLabel context (AppendSymbol "file-" a) (EnvironmentFile a)

mkFileLabel :: Label (AppendSymbol "file-" a) (EnvironmentFile a)
mkFileLabel = Label
