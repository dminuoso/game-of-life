module Automaton
    ( Pos
    , points
    , points'
    , runAutomaton
    , runAutomaton'
    )
where

-- Extra
import Control.Comonad.Store (Store, experiment)
import Control.Comonad (extract, extend)

-- Project
import Pos
import Grid

alive :: Bool
alive = True

dead :: Bool
dead = False

-- Given a lower right corner, this produces a grid of positions.
points :: Pos -> [[Pos]]
points (V2 mx my) = [ [ V2 x y | x <- [0..mx] ] | y <- [0..my]]

points' :: Pos -> [[Pos]]
points' (V2 mx my) = [ [ V2 y x | x <- [0..mx] ] | y <- [0..my]]
-- | Produces a list of neighboring positions.
neighbours :: Pos -> [Pos]
--neighbours p = [p `mappend` V2 x y | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
{- We need to memoize this. Can we use an IORef Ma-}

neighbours p = mappend p <$> offsets

{-# ANN offsets "HLint: ignore Redundant bracket" #-}
offsets = [ V2 (-1) (-1), V2 (-1) (-0), V2 (-1) ( 1)
          , V2 ( 0) (-1),               V2 ( 0) ( 1)
          , V2 ( 1) (-1), V2 ( 1) (-0), V2 ( 1) ( 1)
          ]


-- | Counts live neighboring cells
neighboring :: Store Pos Bool -> Int
neighboring = count . experiment neighbours where
    count = length . filter id

-- | Rule for Conway's Game of Life
rule :: Store Pos Bool -> Bool
rule s = case count of
    2 -> extract s -- Stable
    3 -> alive     -- Stable/Reproduction
    _ -> dead      -- Over-/Underpopulation
  where
    count = neighboring s

-- | Run a cycle
runAutomaton :: Store Pos Bool -> Store Pos Bool
runAutomaton = extend rule

-- | Runs a cycle and materializes the store up to the passed coordinate.
runAutomaton' :: Pos -> Store Pos Bool -> Store Pos Bool
runAutomaton' p = realize p . runAutomaton
