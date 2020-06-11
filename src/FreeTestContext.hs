{-# LANGUAGE PolyKinds #-}
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

module FreeTestContext where

import Control.Monad.Free

data Example a = Example a

data (path :: *) :> (a :: *) = path :> a

data TestCommand context next =
  Before String (Test context ()) next
  -- | Introduce String (context -> IO (a :> context)) (Test (a :> context)) next
  | IntroduceInt String (context -> IO (Int :> context)) (Test (Int :> context) ()) next
  | Describe String (Test context ()) next
  | It String (Example context) next
  deriving (Functor)

type Test a = Free (TestCommand a)

describe :: String -> Test context () -> Test context ()
describe label p = liftF (Describe label p ())

before :: String -> Test context () -> Test context ()
before label p = liftF (Before label p ())

introduceInt :: String -> (context -> IO (Int :> context)) -> Test (Int :> context) () -> Test context ()
introduceInt label introducer p = liftF (IntroduceInt label introducer p ())

it :: String -> Example context -> Test context ()
it label ex = liftF (It label ex ())

test :: Test () ()
test = do
  before "asdf" $ do
    it "does another thing" (Example ())

  introduceInt "asdf" (\() -> return (42 :> ())) $ do
    it "uses the int" (Example (24 :> ()))

  it "does a thing" (Example ())

  describe "it does this thing also" $ do
    it "does a sub-test" (Example ())
