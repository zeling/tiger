{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Cata
       ( Mu (..)
       , Algebra
       , cata ) where

newtype Mu f = Mu { out :: f (Mu f) }

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Mu f -> a
cata alg = alg . fmap (cata alg) . out

instance (Show (f (Mu f))) => Show (Mu f) where
  showsPrec i (Mu f) = showsPrec i f


deriving instance (Ord (f (Mu f))) => Ord (Mu f)
deriving instance (Eq (f (Mu f))) => Eq (Mu f)
deriving instance (Read (f (Mu f))) => Read (Mu f)
