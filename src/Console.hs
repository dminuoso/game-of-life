{-# LANGUAGE FlexibleContexts #-}
module Console where

-- Base
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Data.Foldable
import Data.List
import System.IO (hFlush)

-- Extra
import Control.Comonad.Store

-- Project
import Automaton
import Pos
import Shapes

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

resetCursor :: IO ()
resetCursor = putStr "\ESC[0;0J"

-- Are these VT100 escape sequences?
hideCursor :: IO ()
hideCursor = putStr "\ESCB[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

toC :: Bool -> Char
toC True  = 'x'
toC False = '.'

printStore :: Pos -> Store Pos Bool -> IO ()
printStore lr s = traverse_ putStrLn (transpose chars) where
    peekIn = flip peek
    chars  = (fmap . fmap) (toC . peekIn s) (points lr)

run :: IO ()
run = do
    clearScreen
    hideCursor
    step $ iterate (runAutomaton' (V2 5 5)) toad
  where
    step (x:xs) = do
        resetCursor
        --threadDelay 50000
        printStore (V2 5 5) x
        step xs
