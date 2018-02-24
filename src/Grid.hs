module Grid
    ( realize
    )
where


-- Base
import Control.Monad (join)
import Data.Word (Word64)
import Data.Vector.Unboxed (Vector, (!), fromList)
import Data.Bits (testBit)

-- Extras
import Control.Comonad.Store

-- Project
import Pos

data Grid = Grid { corner :: V2 Int
                 , datum  :: Vector Word64
                 } deriving (Show)

-- | Slices a list into n-sized chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l  = take n l : chunksOf n (drop n l)

-- | Packs a nested listed into an Unboxed Vector Word64.
pack :: [[Bool]] -> Vector Word64
pack cells = vec where
    vec      = fromList chunks
    bits     = join cells
    chunks   = (fromBitstring id . reverse) <$> chunksOf 64 bits

-- | Produces a Store function for a Grid packed table.
testCell :: Grid -> Pos -> Bool
testCell (Grid (V2 mx my) vec) (V2 x y)
    | x > mx || x < 0 = False
    | y > my || y < 0 = False
    | otherwise      =  (vec ! base) `testBit` offset
  where
    (base, offset) = bitIndex `divMod` 64
    bitIndex       = (x * (my+1)) + y

materialize :: Pos -> Store Pos Bool -> [[Bool]]
materialize (V2 mx my) s = (fmap . fmap) (`peek` s) coords where
    coords  = [ [ V2 x y | y <- [0..my] ] | x <- [0..mx]]

-- | Realizes a Store
realize :: Pos -> Store Pos Bool -> Store Pos Bool
realize p s = store (testCell grid) (V2 0 0) where
    grid    = Grid p (pack cells)
    cells   = materialize p s

realizing :: Pos -> Store Pos Bool -> ([[Bool]], Store Pos Bool)
realizing p s = (cells, game) where
    game  = store (testCell grid) (V2 0 0)
    grid  = Grid p (pack cells)
    cells = materialize p s

-- | Turns a little endian Bitstring like into an arbitrary Integral.
fromBitstring :: (Integral b) => (a -> Bool) -> [a] -> b
fromBitstring p = f . reverse  where
    f []     = 0
    f (x:xs) | p x        = 1 + 2 * f xs
             | otherwise  = 0 + 2 * f xs

toBin :: (Integral a) => a -> String
toBin 0 = ""
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ "1"
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ "0"
