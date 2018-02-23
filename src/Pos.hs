{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Pos
    ( Pos
    , V2(..)
    )
where

import Control.Monad

type Pos = V2 Int

data V2 a = V2 !a !a deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
instance Applicative V2 where
    pure = join V2
    V2 f1 f2 <*> V2 x1 x2 = V2 (f1 x1) (f2 x2)

instance (Num a) => Monoid (V2 a) where
    mempty = V2 0 0
    V2 x1 x2 `mappend` V2 y1 y2 = V2 (x1 + y1) (x2 + y2)
