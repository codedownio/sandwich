{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
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

module FreeTestContextIntroduce where

import Control.Monad.Free
import Control.Monad.Free.TH

data Example a = Example a

data (path :: *) :> (a :: *) = path :> a

data TestCommand introducing context next =
  Before String (Test () context ()) next 
  | Introduce String (context -> IO (introducing :> context)) (Test introducing (introducing :> context) ()) next
  | IntroduceInt String (context -> IO (Int :> context)) (Test () (Int :> context) ()) next
  | Describe String (Test () context ()) next
  | It String (Example context) next
  deriving (Functor)

type Test introducing context = Free (TestCommand introducing context)

$(makeFree ''TestCommand)
  
test :: Test () () ()
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
