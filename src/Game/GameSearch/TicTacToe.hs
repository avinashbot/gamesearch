{-# LANGUAGE MultiParamTypeClasses #-}

module Game.GameSearch.TicTacToe (start, Draw(..), State, Player, winner) where

import Data.Array.IArray (Array, array, (!), (//))
import Data.Maybe        (isJust, isNothing, listToMaybe, mapMaybe)
import Game.GameSearch   (Spec (..))

data Player = Max | Min deriving (Eq, Ord, Show)
data Space = Occupied Player | Empty deriving (Eq, Show)

data State = State
    { grid :: Array (Int, Int) Space
    , turn :: Player
    } deriving (Show)

newtype Draw = Draw (Int, Int) deriving (Eq, Ord, Show)

instance Spec State Draw Player where
    -- Returns positions where the columns are not filled up.
    actions s
        | isJust (winner s) = []
        | otherwise = map (\pos -> (1.0, Draw pos)) $
                      filter (\a -> grid s ! a == Empty)
                      [(a, b) | a <- [0..2], b <- [0..2]]

    player = turn

    -- Returns a payout of 1 if we won, 0 if we lost.
    payouts s
        | winner s == Just Max = [(Max, 1.0), (Min, -1.0)]
        | winner s == Just Min = [(Max, -1.0), (Min, 1.0)]
        | isNothing (winner s) && null (actions s) = [(Max, 0.0), (Min, -0.0)]

    -- Applies action a to board b.
    apply (Draw p) s = State
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

checkMatrix :: [[(Int, Int)]]
checkMatrix =
    [ [(0, 0), (0, 1), (0, 2)]
    , [(1, 0), (1, 1), (1, 2)]
    , [(2, 0), (2, 1), (2, 2)]
    , [(0, 0), (1, 0), (2, 0)]
    , [(0, 1), (1, 1), (2, 1)]
    , [(0, 2), (1, 2), (2, 2)]
    , [(0, 0), (1, 1), (2, 2)]
    , [(0, 2), (1, 1), (2, 0)]
    ]

isLine :: Array (Int, Int) Space -> [(Int, Int)] -> Maybe Player
isLine arr check
    | all (== Occupied Min) $ map (arr !) check = Just Min
    | all (== Occupied Max) $ map (arr !) check = Just Max
    | otherwise = Nothing
