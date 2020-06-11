{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

module FreeTestContextIntroduceGADT where

import Control.Monad.Free
import Control.Monad.Free.TH

data Example a = Example a

data (path :: *) :> (a :: *) = path :> a

data TestCommand context next where
  Before :: String -> (Test context ()) -> next -> TestCommand context next
  Introduce :: String -> (context -> IO (intro :> context)) -> (Test (intro :> context) ()) -> next -> TestCommand context next
  IntroduceInt :: String -> (context -> IO (Int :> context)) -> (Test (Int :> context) ()) -> next -> TestCommand context next
  Describe :: String -> (Test context ()) -> next -> TestCommand context next
  It :: String -> (Example context) -> next -> TestCommand context next

deriving instance Functor (TestCommand n)

type Test context = Free (TestCommand context)

$(makeFree_ ''TestCommand)
  
test :: Test () () 
test = do
  before "asdf" $ do
    it "does another thing" (Example ())

  introduceInt "asdf" (\() -> return (42 :> ())) $ do
    it "uses the int" (Example (24 :> ()))

  introduce "Intro a string" (\() -> return ("foo" :> ())) $ do
    it "uses the int" (Example ("foo" :> ()))
  
  it "does a thing" (Example ())

  describe "it does this thing also" $ do
    it "does a sub-test" (Example ())
