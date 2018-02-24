{-# LANGUAGE FlexibleContexts #-}

module Shapes
    ( toad
    , pentadecathlon
    , gun
    )
where

-- Core
import Data.Maybe (fromMaybe)

-- Extra
import Control.Comonad.Store (Store, store)

-- Project
import Pos

nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 1 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs

shape :: [String] -> Store Pos Bool
shape text = store (testCell text) (V2 0 0)

testCell :: [String] -> Pos -> Bool
testCell text (V2 x y) = fromMaybe False status where
    status = (=='x') <$> (nth x text >>= nth y)

toad :: Store Pos Bool
toad =  shape [ "......"
              , ".xx..."
              , ".xx..."
              , "...xx."
              , "...xx."
              , "......"
              ]

pentadecathlon :: Store Pos Bool
pentadecathlon = shape [ "................................."
                       , "................................."
                       , "................................."
                       , "................................."
                       , "................................."
                       , "................................."
                       , "..............x....x............."
                       , "............xx.xxxx.xx..........."
                       , "..............x....x............."
                       , "................................."
                       , "................................."
                       , "................................."
                       , "................................."
                       , "................................."
                       ]

gun :: Store Pos Bool
gun = shape
    [ "......................................."
    , "......................................."
    , "......................................."
    , "..........................x............"
    , "........................x.x............"
    , "..............xx......xx............xx."
    , ".............x...x....xx............xx."
    , "..xx........x.....x...xx..............."
    , "..xx........x...x.xx....x.x............"
    , "............x.....x.......x............"
    , ".............x...x....................."
    , "..............xx......................."
    , "......................................."
    ]
