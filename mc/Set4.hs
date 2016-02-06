{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set4 where

import           MCPrelude
import           Set2

-- 403

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- 404

newtype Gen a = Gen (Seed -> (a, Seed))

-- (Seed -> (a,Seed)) -> (a -> (Seed -> (a,Seed))) -> Seed -> (a,Seed)

instance Monad Gen where
  bind (Gen g) f s = let (rv,ns) = g s in f ns

instance Monad Maybe where
  bind Nothing  _ = Nothing
  bind (Just x) f = f x
  return          = Just

instance Monad [] where
  bind     = flip concatMap
  return x = [x]

