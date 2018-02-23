{-# LANGUAGE FlexibleContexts #-}
module Console where

-- Base
import Data.Foldable
import Data.List
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Data.Time (getCurrentTime, diffUTCTime)

-- Extra
import Control.Comonad.Store
import System.Console.ANSI (setCursorPosition, hideCursor)

-- Project
import Automaton
import Pos
import Shapes

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

resetCursor :: IO ()
resetCursor = putStr "\033[0;0f"

--hideCursor :: IO ()
--hideCursor = putStr "\u001B[?25l"

toC :: Bool -> Char
toC True  = 'x'
toC False = ' '

printStore :: Pos -> Store Pos Bool -> IO ()
printStore lr s = putStr ((intercalate "\n\r" . transpose) chars) where
    peekIn = flip peek
    chars  = (fmap . fmap) (toC . peekIn s) (points lr)

runTest :: IO ()
runTest = do
    hideCursor
    clearScreen
    step xs
  where
    xs = iterate (runAutomaton' (V2 40 40)) gun
    step (x:xs) = do
        setCursorPosition 0 0
        threadDelay 50000
        start <- getCurrentTime
        printStore (V2 40 40) x
        end <- getCurrentTime
        print (diffUTCTime end start)
        step xs

run :: Pos -> Store Pos Bool -> IO ()
run p s = traverse_ step g where
    g = iterate (runAutomaton' p) s
    step s = do
        threadDelay 50000
        setCursorPosition 0 0
        start <- getCurrentTime
        printStore p s
        end <- getCurrentTime
        print (diffUTCTime end start)
