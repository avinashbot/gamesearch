{-# LANGUAGE MultiParamTypeClasses #-}

module Game.GameSearch.Mancala
    ( State(..)
    , Player(..)
    , start
    ) where

import qualified Data.Sequence   as Seq
import           Game.GameSearch (Spec (..))

-- A mancala board stores the number of houses on each side along with the
-- number of seeds currently in each house.
-- The size of the board (houses * 2 + 2) is guaranteed to be even.
type Board = Seq.Seq Int

-- A player is either us (Max) or the opponent (Min)
data Player
    = Max
    | Min
    deriving (Eq, Ord, Show)

-- Represents the current player and the state.
data State =
    State Board
          Player

instance Show State where
    show (State b _) =
        "[" ++
        show (b ! scoring Min b) ++
        "] " ++
        foldl
            (\sm ix -> sm ++ "[" ++ show (b ! ix) ++ "] ")
            ""
            (reverse (houses Min b)) ++
        "\n    " ++
        foldl (\sm ix -> sm ++ "[" ++ show (b ! ix) ++ "] ") "" (houses Max b) ++
        "[" ++ show (b ! scoring Max b) ++ "]"

instance Spec State Int Player where
    actions = availDrops
    player (State _ player) = player
    apply = placeFrom
    payouts (State board player)
        | score Max board > score Min board = [(Max, 1.0), (Min, -1.0)]
        | score Max board < score Min board = [(Min, 1.0), (Max, -1.0)]
        | otherwise = [(Max, 0.0), (Min, 0.0)]

--
(!) :: Seq.Seq a -> Int -> a
(!) = Seq.index

--
start h s = State (start' h s) Max

--
start' houses seeds = Seq.update (scoring Min board2) 0 board2
  where
    board2 = Seq.update (scoring Max board) 0 board
    board = Seq.replicate (houses * 2 + 2) seeds

-- Returns the opponent of the player
opponent :: Player -> Player
opponent Max = Min
opponent Min = Max

-- The oppsite house of a position.
opposite :: Board -> Int -> Int
opposite board pos = (Seq.length board - 2) - pos

-- Returns the index of the store of the player.
scoring :: Player -> Board -> Int
scoring Max board = (Seq.length board `div` 2) - 1
scoring Min board = Seq.length board - 1

-- Returns whether the position is in a house of the player
houseof :: Player -> Board -> Int -> Bool
houseof Max board pos = pos > 0 && pos < scoring Max board
houseof Min board pos = pos > scoring Max board && pos < scoring Min board

-- Returns the next position a player can drop a seed in.
next :: Player -> Board -> Int -> Int
next player board pos =
    mod
        (if (pos + 1) == scoring (opponent player) board
             then pos + 2
             else pos + 1)
        (Seq.length board)

-- Place a number of seeds at a position for a player on a board.
-- Returns the board and the final position, for efficiency.
place :: Int -> Int -> Player -> Board -> (Board, Int)
place 1 pos ply board = (Seq.adjust (+ 1) pos board, pos)
place count pos ply board =
    let (updated, lastPos) = place (count - 1) (next ply board pos) ply board
    in (Seq.adjust (+ 1) pos updated, lastPos)

-- Clear all seeds from a position
clear :: Int -> Board -> Board
clear = Seq.update 0

-- Returns the player who will play the next turn based on the ending position
-- of a move.
nextturn :: Int -> Board -> Player -> Player
nextturn finalpos board ply
    -- If the final position is the own player's store.
    | finalpos == scoring ply board = ply
    -- Otherwise, it's the opponent's turn.
    | otherwise = opponent ply

-- The player's score.
score :: Player -> Board -> Int
score ply board =
    (board ! scoring ply board) + sum (map (board !) (houses ply board))

houses :: Player -> Board -> [Int]
houses Max board = [0 .. (scoring Max board - 1)]
houses Min board = [(scoring Max board + 1) .. (scoring Min board - 1)]

-- TODO: cutoff when you have most of the points
availDrops :: State -> [(Double, Int)]
availDrops (State board ply) =
    map (\i -> (1.0, i)) $ filter (\i -> board ! i > 0) (houses ply board)

-- Apply :|
placeFrom :: Int -> State -> State
placeFrom pos (State board ply) = State checked (nextturn final checked ply)
  where
    checked =
        if houseof ply board final &&
           board ! final == 0 && board ! opposite board final > 0
            then Seq.update
                     (opposite updated final)
                     0
                     (Seq.adjust
                          (+ ((updated ! opposite updated final) + 1))
                          (scoring ply updated)
                          (Seq.update final 0 updated))
            else updated
    (updated, final) = place (board ! pos) (next ply board pos) ply cleared
    cleared = Seq.update pos 0 board
