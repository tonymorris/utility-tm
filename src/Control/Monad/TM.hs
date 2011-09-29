-- | Monadic combinators missing from the standard library
module Control.Monad.TM
(
  (.=<<.)
,  (.>>=.)
, anyM
, allM
, findM
) where

import Control.Applicative
import Data.Traversable
import Control.Monad

-- | Lifting bind into a monad. Often denoted /concatMapM/.
(.=<<.) ::
  (Applicative q, Monad m, Traversable m) =>
  (a -> q (m b))
  -> m a
  -> q (m b)
(.=<<.) f =
  fmap join . traverse f

-- | Lifting bind into a monad. Often denoted /concatMapM/.
(.>>=.) ::
  (Applicative q, Monad m, Traversable m) =>
  m a
  -> (a -> q (m b))
  -> q (m b)
(.>>=.) =
  flip (.=<<.)

-- | Existential quantification.
anyM ::
  Monad m =>
  (a -> m Bool)
  -> [a]
  -> m Bool
anyM _ []     =
  return False
anyM f (a:as) =
  do z <- f a
     if z
       then return True
       else anyM f as

-- | Universal quantification.
allM ::
  Monad m =>
  (a -> m Bool)
  -> [a]
  -> m Bool
allM _ []     =
  return True
allM f (a:as) =
  do z <- f a
     if z
       then allM f as
       else return False

-- | Find an element satisfying a predicate
findM ::
  Monad m =>
  (a -> m Bool)
  -> [a]
  -> m (Maybe a)
findM _ [] =
  return Nothing
findM f (x:xs) =
  do b <- f x
     if b
       then
         return (Just x)
       else
         findM f xs
