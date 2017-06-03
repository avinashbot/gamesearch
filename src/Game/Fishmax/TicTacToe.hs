{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.TicTacToe (start, Draw(..)) where

import Data.Array.IArray       (Array, array, (!), (//))
import Data.Maybe              (isJust, isNothing, listToMaybe, mapMaybe)
import Game.Fishmax.TreeSearch (Action, Spec (..))

data Player = Max | Min deriving (Eq, Show)
data Space = Occupied Player | Empty deriving (Eq, Show)

data State = State
    { grid :: Array (Int, Int) Space
    , turn :: Player
    } deriving (Show)

newtype Draw = Draw (Int, Int) deriving (Eq, Ord, Show)
instance Action Draw

instance Spec State Draw where
    -- Returns positions where the columns are not filled up.
    actions s
        | isJust (winner s) = []
        | otherwise = map Draw $
                      filter (\a -> grid s ! a == Empty)
                      [(a, b) | a <- [0..2], b <- [0..2]]

    -- Returns a payout of 1 if we won, 0 if we lost.
    payout s
        | winner s == Just Max = Just 1.0
        | winner s == Just Min = Just (-1.0)
        | isNothing (winner s) && null (actions s) = Just 0.0
        | otherwise = Nothing

    -- Applies action a to board b.
    apply (Draw p) s = State
        { grid = grid s // [(p, Occupied (turn s))]
        , turn = if turn s == Max then Min else Max
        }

start :: State
start = State
    { grid = array ((0, 0), (2, 2)) [((a, b), Empty) | a <- [0..2], b <- [0..2]]
    , turn = Min
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
