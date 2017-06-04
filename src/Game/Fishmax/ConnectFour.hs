{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.ConnectFour (start, State(..), Drop(..)) where

import Data.List               (find)
import Data.Maybe              (isJust, fromJust)
import Data.Ix                 (range)
import Data.Array.IArray       (Array, array, bounds, (!), (//))
import Game.Fishmax.TreeSearch (Spec(..), Action)

data Player = Max | Min deriving (Eq, Show)
data Space = Occupied Player | Empty | Null deriving (Eq, Show)

data State = State
    { grid :: Array (Int, Int) Space
    , turn :: Player
    , winner :: Maybe Player
    } deriving (Show)

newtype Drop = Drop Int deriving (Eq, Ord, Show)
instance Action Drop

instance Spec State Drop where
    -- Returns positions where the columns are not filled up.
    actions s
        | isJust (winner s) = []
        | otherwise         = availDrops (grid s)

    -- Returns a payout of 1 if we won, 0 if we lost.
    payout s
        | null (actions s) = Just (payoutOf (winner s))
        | otherwise        = Nothing

    -- Applies action a to board b.
    apply (Drop c) s =
        State
        { grid   = newGrid
        , turn   = if turn s == Max then Min else Max
        , winner = if winningMove then Just (turn s) else Nothing
        } where
            dropPos     = (droppedRow (grid s) c, c)
            newGrid     = grid s // [(dropPos, Occupied (turn s))]
            winningMove = isWinningMove newGrid dropPos (turn s)

-- Return the starting state.
start :: State
start = State
    { grid = array ((0, 0), (5, 6)) [((r, c), Empty) | r <- [0..5], c <- [0..6]]
    , turn = Max
    , winner = Nothing
    }

-- Returns the positions where the columns aren't filled up.
availDrops :: Array (Int, Int) Space -> [Drop]
availDrops grid = map Drop $ filter (\col -> (grid ! (0, col)) == Empty) [0..6]

-- Returns the playout of a possible winner.
payoutOf :: Maybe Player -> Double
payoutOf (Just Max) = 1.0
payoutOf (Just Min) = -1.0
payoutOf Nothing    = 0.0

-- Get the row that the piece settles in if we drop it at that column.
droppedRow :: Array (Int, Int) Space -> Int -> Int
droppedRow grid col =
    if isJust firstDropped then fromJust firstDropped - 1 else 5
    where
        firstDropped = find (\row -> (grid ! (row, col)) /= Empty) [0..5]

-- An O(1) win check for connect 4 around a piece that is independent of board
-- size.
isWinningMove :: Array (Int, Int) Space -> (Int, Int) -> Player -> Bool
isWinningMove grid (r, c) player =
    any (all (\(i, j) -> (grid `safe` (r + i, c + j)) == Occupied player))
        [ [(0, 0),  (0, 1),  (0, 2),  (0, 3)]
        , [(0, -1), (0, 0),  (0, 1),  (0, 2)]
        , [(0, -2), (0, -1), (0, 0),  (0, 1)]
        , [(0, -3), (0, -2), (0, -1), (0, 0)]

        , [(0, 0),  (1, 0),  (2, 0),  (3, 0)]
        , [(-1, 0), (0, 0),  (1, 0),  (2, 0)]
        , [(-2, 0), (-1, 0), (0, 0),  (1, 0)]
        , [(-3, 0), (-2, 0), (-1, 0), (0, 0)]

        , [(0, 0),   (1, 1),   (2, 2),   (3, 3)]
        , [(-1, -1), (0, 0),   (1, 1),   (2, 2)]
        , [(-2, -2), (-1, -1), (0, 0),   (1, 1)]
        , [(-3, -3), (-2, -2), (-1, -1), (0, 0)]

        , [(0, 0),  (-1, 1), (-2, 2), (-3, 3)]
        , [(1, -1), (0, 0),  (-1, 1), (-2, 2)]
        , [(2, -2), (1, -1), (0, 0),  (-1, 1)]
        , [(3, -3), (2, -2), (1, -1), (0, 0)]
        ]

-- Safe array lookup
safe :: Array (Int, Int) Space -> (Int, Int) -> Space
safe arr (row, col)
    | row < minRow || row > maxRow || col < minCol || col > maxRow = Null
    | otherwise = arr ! (row, col)
    where ((minRow, minCol), (maxRow, maxCol)) = bounds arr
