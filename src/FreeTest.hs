{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
module Test.Hspec.Core.FreeTest where

import Control.Monad.Free

data Example = Example

data TestCommand next =
  Before String (Test ()) next
  | Describe String (Test ()) next
  | It String Example next
  deriving (Functor)

type Test = Free TestCommand

describe :: String -> Test () -> Test ()
describe label p = liftF (Describe label p ())

before :: String -> Test () -> Test ()
before label p = liftF (Before label p ())

it :: String -> Example -> Test ()
it label ex = liftF (It label ex ())

test :: Test ()
test = do
  before "asdf" $ do
    it "does another thing" Example

  it "does a thing" Example

  describe "it does this thing also" $ do
    it "does a sub-test" Example
