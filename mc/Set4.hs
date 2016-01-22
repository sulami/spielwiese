{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set4 where

import           MCPrelude

-- 403

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

