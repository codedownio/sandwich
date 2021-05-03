---
id: contexts
title: Contexts
sidebar_label: Contexts
slug: /contexts
---

import useBaseUrl from '@docusaurus/useBaseUrl';

One of Sandwich's most powerful features is the ability to introduce *contexts* in tests. A context is simply a labeled dependency, which can be obtained in a test using the `getContext` function:

```haskell
it "tests the database" $ do
  db <- getContext database
  queryUser db "user1" >>= (`shouldBe` mockUser1)
```

Sandwich gives you the tools to introduce (and gracefully tear down) contexts for use in your tests while keeping the plumbing nicely hidden.  The type system enforces that a test has all the contexts it needs.

## Built-in contexts

Sandwich provides some contexts automatically. For example, you can retrieve the on-disk folder for a given node by calling `getCurrentFolder`. This can be useful if you want to save custom logs, screenshots, etc. to the folder.

```haskell
it "saves a picture of the login page using Selenium" $ do
  openPage "/login"
  Just folder <- getCurrentFolder
  screenshot >>= B.writeFile (folder </> "screenshot.png")
```

Note that `getCurrentFolder` returns a `Maybe FilePath`. It will be `Nothing` if your tests are run without an on-disk folder, or if the particular node in question is configured not to create a folder in its [node options](http://localhost:3000/docs/node_options).

Another built-in function is `getRunRoot`, which will return the *root* of the on-disk test tree. This can be useful if you want to store test-wide artifacts there. Similar caveats apply when Sandwich is configured to run without on-disk state.


## Introducing your own contexts

Suppose we want to introduce a mock database into some tests. First, we define a label for it. The label represents the mapping between a type-level string and the type of the context. You can find the full working example for this section [here](https://github.com/codedownio/sandwich/blob/master/sandwich-demos/demos/contexts/Main.hs).

```haskell
{-# LANGUAGE DataKinds #-}

data DatabaseContext = MySQLDatabaseContext | SqliteDatabaseContext
  deriving Show

database = Label :: Label "database" DatabaseContext
```

Next, we write the introduce node. We choose to use [introduceWith](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:introduceWith), because it allows us to use the `bracket` pattern to create and then tear down our database. You can imagine IO actions happening here.

```haskell
introduceDatabase = introduceWith "Introduce database" database $ \action ->
  bracket (debug "Spinning up DB..." >> return MySQLDatabaseContext)
          (\db -> debug "Tearing down DB..." >> return ())
          (void . action)
```

Inside the test, we can use `getContext` to get the context and do things with it. 

```haskell
contextsDemo :: TopSpec
contextsDemo = describe "Contexts" $ do
  introduceDatabase $ do
    it "Uses the database" $ do
      db <- getContext database
      info [i|Got database: '#{db}'|]
```

## The HasX pattern for context dependencies

Now let's decouple the introduce node from the test (full working example [here](https://github.com/codedownio/sandwich/blob/master/sandwich-demos/demos/context-dependencies/Main.hs)). First, we'll define the type of a spec that depends on a database. To do this, we'll need a HasX-style constraint type.

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
type HasDatabase context = HasLabel context "database" DatabaseContext
```

Now, we can use this to define the type of our spec.

```haskell
{-# LANGUAGE RankNTypes #-}
type DatabaseSpec = forall context. (HasDatabase context) => SpecFree context IO ()
```

Now that we have the spec type, we can start writing specs. You can imagine these living in separate files. These tests don't care about the exact context they're run with, as long as it has a `database` available.

```haskell
-- In DatabaseTest1.hs
databaseTest1 :: DatabaseSpec
databaseTest1 = do
  it "uses the database 1" $ getContext database >>= \db -> 
    info [i|Got database: '#{db}'|]

-- In DatabaseTest2.hs
databaseTest2 :: DatabaseSpec
databaseTest2 = do
  it "uses the database 2" $ getContext database >>= \db -> 
    info [i|Got database: '#{db}'|]
```

Now, in your main test file, you can import both of these tests and run them in the same test tree.

```haskell
contextDepsDemo :: TopSpec
contextDepsDemo = describe "Context dependencies" $ do
  introduceDatabase $ do
    databaseTest1
    databaseTest2
```

Or, if you want better isolation, you can rearrange this to create a separate database for each subtree.

```haskell
contextDepsDemo :: TopSpec
contextDepsDemo = describe "Context dependencies" $ do
  introduceDatabase databaseTest1
  introduceDatabase databaseTest2
```

Either way, the type system ensures that your tests have the contexts they need.

## Contexts depending on other contexts

We can use the same HasX trick to write contexts that depend on other contexts. For example, suppose you're testing a server and the server depends on a database. You need a database to exist first in order to create the server, and you want both the server and the database available to your tests.

First, let's introduce the `Server` type and an introduce function for it. This introduce function is special because it contains a `getContext` call to retrieve the database and use it to make the server.

```haskell
data Server = Server DatabaseContext deriving Show
server = Label :: Label "server" Server

introduceServer = introduceWith "Introduce server" server $ \action -> do
  bracket (do
              db <- getContext database
              debug "Spinning up server..."
              return $ Server db
          )
          (\server -> debug "Tearing down server..." >> return ())
          (void . action)
```

Now, we need to write `introduceServer` nested inside a `introduceDatabase` node:

```haskell
contextNestedDepsDemo :: TopSpec
contextNestedDepsDemo = describe "Nested dependencies" $ do
  introduceDatabase $
    introduceServer $
      it "uses the server" $ do
        s <- getContext server
        debug [i|Got server: #{s}|]
```

Note that it's usually easiest to let GHC infer the type signature of `introduceServer`. If you do need to write out the type signature, it can be a little bit verbose since it needs to use the underlying context constructors and put appropriate constraints on the base monad. For this example, the signature for this example might look like this:

```haskell
introduceServer :: (HasDatabase context, MonadIO m, MonadBaseControl IO m)
  => SpecFree (LabelValue "server" Server :> context) m () -> SpecFree context m ()
```

The full code for this example can be found [here](https://github.com/codedownio/sandwich/blob/master/sandwich-demos/demos/context-nested-dependencies/Main.hs).
