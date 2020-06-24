-- |

module Test.Sandwich.Util where

whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) action = action x
whenLeft (Right _) _ = return ()
