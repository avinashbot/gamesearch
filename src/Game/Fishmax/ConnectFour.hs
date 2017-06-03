{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.ConnectFour (start, State(..), Drop(..)) where

import Data.List               (find)
import Data.Maybe              (isJust, fromJust)
import Data.Ix                 (range)
import Data.Array.IArray       (Array, array, bounds, (!), (//))
import Game.Fishmax.TreeSearch (Spec(..), Action)

data Player = Max | Min deriving (Eq, Show)
data Space = Occupied Player | Empty deriving (Eq, Show)

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
        { grid   = newGrd
        , turn   = if turn s == Max then Min else Max
        , winner = if winningTurn then Just (turn s) else Nothing
        } where
            newGrd = grid s // [((droppedRow (grid s) c, c), Occupied (turn s))]
            winningTurn = isWinner newGrd (turn s)

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

-- Returns the playout of a possible winner, assuming a tie as a loss.
payoutOf :: Maybe Player -> Double
payoutOf (Just Max) = 1.0
payoutOf (Just Min) = -1.0
payoutOf Nothing    = -1.0

-- Get the row that the piece settles in if we drop it at that column.
droppedRow :: Array (Int, Int) Space -> Int -> Int
droppedRow grid col =
    if isJust firstDropped then fromJust firstDropped - 1 else 5
    where
        firstDropped = find (\row -> (grid ! (row, col)) /= Empty) [0..5]

-- Check around a position for a connect 4.
isWinner :: Array (Int, Int) Space -> Player -> Bool
isWinner g p = horzWin g p || vertWin g p || descDiagWin g p || ascDiagWin g p

-- Check for a horizontal line.
horzWin :: Array (Int, Int) Space -> Player -> Bool
horzWin g p = any check (range (minBounds, (maxRow, maxCol - 3))) where
    (minBounds, (maxRow, maxCol)) = bounds g
    check (r, c) = all (\i -> (g ! i) == Occupied p)
                       [(r, c), (r, c+1), (r, c+2), (r, c+3)]

-- Check for a vertical line.
vertWin :: Array (Int, Int) Space -> Player -> Bool
vertWin g p = any check (range (minBounds, (maxRow - 3, maxCol))) where
    (minBounds, (maxRow, maxCol)) = bounds g
    check (r, c) = all (\i -> (g ! i) == Occupied p)
                       [(r, c), (r+1, c), (r+2, c), (r+3, c)]

-- Check for a descending diagonal line.
descDiagWin :: Array (Int, Int) Space -> Player -> Bool
descDiagWin g p = any check (range (minBounds, (maxRow - 3, maxCol - 3))) where
    (minBounds, (maxRow, maxCol)) = bounds g
    check (r, c) = all (\i -> (g ! i) == Occupied p)
                       [(r, c), (r+1, c+1), (r+2, c+2), (r+3, c+3)]

-- Check for an ascending diagonal line.
ascDiagWin :: Array (Int, Int) Space -> Player -> Bool
ascDiagWin g p = any check (range ((minRow + 3, minCol), (maxRow, maxCol - 3))) where
    ((minRow, minCol), (maxRow, maxCol)) = bounds g
    check (r, c) = all (\i -> (g ! i) == Occupied p)
                       [(r, c), (r-1, c+1), (r-2, c+2), (r-3, c+3)]
