{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich where

import Control.Monad.Free
import Control.Monad.Free.TH
import Test.Sandwich.Types.Example


data (a :: *) :> (b :: *) = a :> b

data TestCommand context next where
  Before :: String -> (Test context ()) -> next -> TestCommand context next
  Introduce :: String -> (context -> IO (intro :> context)) -> (Test (intro :> context) ()) -> next -> TestCommand context next
  Describe :: String -> (Test context ()) -> next -> TestCommand context next
  It :: (Example e) => String -> e -> next -> TestCommand context next

deriving instance Functor (TestCommand n)

type Test context = Free (TestCommand context)

$(makeFree_ ''TestCommand)
  
test :: Test () () 
test = do
  before "asdf" $ do
    it "does another thing" Dummy

  introduce "Intro a string" (\() -> return ("foo" :> ())) $ do
    it "uses the int" Dummy
  
  it "does a thing" Dummy

  describe "it does this thing also" $ do
    it "does a sub-test" Dummy
