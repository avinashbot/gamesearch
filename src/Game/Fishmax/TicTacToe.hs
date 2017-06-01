{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.TicTacToe (start, PlaceSymbol(..)) where

import Data.Array.IArray (Array, array, (!), (//))
import Data.Maybe (mapMaybe, listToMaybe, isJust, isNothing)
import Game.Fishmax.TreeSearch (Spec(..), Action)
import Debug.Trace (trace)

data Player = Max | Min deriving (Eq, Show)
data Space = Occupied Player | Empty deriving (Eq, Show)

data State = State { grid :: Array (Int, Int) Space, turn :: Player } deriving (Show)

newtype PlaceSymbol = PlaceSymbol (Int, Int) deriving (Eq, Ord, Show)
instance Action PlaceSymbol

instance Spec State PlaceSymbol where
    -- If the game is over and there's a winner.
    isFinal s = isJust (winner s) || null (actions s)

    -- Returns a payout of 1 if we won, 0 if we lost.
    payout s
        | winner s == Just Max = Just 1.0
        | winner s == Just Min = Just (-1.0)
        | isNothing (winner s) && null (actions s) = Just 0.0
        | otherwise = Nothing

    -- Returns positions where the columns are not filled up.
    actions s = map PlaceSymbol $
                filter (\a -> (grid s ! a) == Empty)
                       [(a, b) | a <- [0..2], b <- [0..2]]

    -- Applies action a to board b.
    apply (PlaceSymbol p) s = s
        { grid = grid s // [(p, Occupied (turn s))]
        , turn = if turn s == Max then Min else Max
        }

start :: State
start = State
    { grid = array ((0, 0), (2, 2)) [((a, b), Empty) | a <- [0..2], b <- [0..2]]
    , turn = Max
    }

winner :: State -> Maybe Player
winner state = listToMaybe $ mapMaybe (isLine (grid state)) checkMatrix

checkMatrix :: [((Int, Int), (Int, Int), (Int, Int))]
checkMatrix =
    [ ((0, 0), (0, 1), (0, 2))
    , ((1, 0), (1, 1), (1, 2))
    , ((2, 0), (2, 1), (2, 2))
    , ((0, 0), (1, 0), (2, 0))
    , ((0, 1), (1, 1), (2, 1))
    , ((0, 2), (1, 2), (2, 2))
    , ((0, 0), (1, 1), (2, 2))
    , ((0, 2), (1, 1), (2, 0))
    ]

isLine :: Array (Int, Int) Space -> ((Int, Int), (Int, Int), (Int, Int)) -> Maybe Player
isLine arr (a, b, c)
    | (arr ! a == arr ! b) && (arr ! b == arr ! c) && (arr ! c == Occupied Min) = Just Min
    | (arr ! a == arr ! b) && (arr ! b == arr ! c) && (arr ! c == Occupied Max) = Just Max
    | otherwise = Nothing
