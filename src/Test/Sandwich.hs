{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Sandwich where

import Control.Monad.Free
import Control.Monad.Free.TH
import GHC.Stack
import qualified Data.List as L
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.ExampleInstances

data (a :: *) :> (b :: *) = a :> b
  deriving Show

data TestCommand context next where
  Before :: String -> (Test context ()) -> next -> TestCommand context next
  Introduce :: (Show intro) => String -> (context -> IO (intro :> context)) -> (Test (intro :> context) ()) -> next -> TestCommand context next
  Describe :: String -> (Test context ()) -> next -> TestCommand context next
  DescribeParallel :: String -> (Test context ()) -> next -> TestCommand context next
  It :: (HasCallStack) => String -> (context -> IO Result) -> next -> TestCommand context next

deriving instance Functor (TestCommand n)

type Test context = Free (TestCommand context)

$(makeFree_ ''TestCommand)

pending _ = return $ Result "pending" (Pending Nothing Nothing)

test :: Test () ()
test = do
  before "asdf" $ do
    it "does another thing" pending

  introduce "Intro a string" (\() -> return ("foo" :> ())) $ do
    it "uses the string" $ \(str :> ()) -> do
      putStrLn $ "Got the string: " <> str
      return $ Result "" Success

  it "does a thing" $ \() -> do
    putStrLn "HI"
    return $ Result "" Success

  describe "it does this thing also" $ do
    it "does a sub-test" pending

  describeParallel "it does this thing also" $ do
    it "does a sub-test 1" pending
    it "does a sub-test 2" pending
    it "does a sub-test 3" pending


showTest :: (Show r, Show context) => Free (TestCommand context) r -> String
showTest = showTest' 0

indentSize :: Int
indentSize = 2

showNode :: (Show r, Show r', Show c, Show c') => Int -> String -> Free (TestCommand c) r -> Free (TestCommand c') r' -> String
showNode indent label subtest next = L.intercalate "\n" $ filter (/= "") [
  (L.replicate indent ' ') <> label
  , showTest' (indent + indentSize) subtest
  , showTest' indent next
  ]

showTest' :: (Show r, Show context) => Int -> Free (TestCommand context) r -> String
showTest' indent (Free (Before l subtest next)) = showNode indent l subtest next
showTest' indent (Free (Introduce l f subtest next)) = showNode indent l subtest next
showTest' indent (Free (Describe l subtest next)) = showNode indent l subtest next
showTest' indent (Free (DescribeParallel l subtest next)) = showNode indent l subtest next
showTest' indent (Free (It l ex next)) = showNode indent l ((return ()) :: Free (TestCommand ()) ()) next
showTest' _ (Pure _) = ""
