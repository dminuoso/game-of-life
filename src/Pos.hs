module Pos
    ( Pos
    , V2(..)
    )
where

-- Core
import Data.Semigroup ((<>), Semigroup)

type Pos = V2 Int

data V2 a = V2 !a !a deriving (Eq, Ord, Show)

instance (Num a) => Semigroup (V2 a) where
    V2 x1 x2 <> V2 y1 y2 = V2 (x1 + y1) (x2 + y2)
