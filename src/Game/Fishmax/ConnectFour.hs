{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.ConnectFour where

import Data.Maybe (isJust, fromJust, mapMaybe)
import Game.Fishmax.TreeSearch (Spec(..), Action)

data Piece = PlayerPiece Int | EmptyPiece deriving (Eq)

data Board = Board
    { gridSize :: (Int, Int)
    , maxPlayer :: Int
    , grid :: [[Piece]]
    , turn :: Int
    , winner :: Maybe Int
    } deriving (Eq)

newtype Drop = Drop Int deriving (Eq, Ord)

instance Spec Board Drop where
    -- If the game is over and there's a winner.
    isFinal b = isJust (winner b)

    -- Returns a payout of 1 if we won, 0 if we lost.
    payout b
        | isFinal b && fromJust (winner b) == 0 = Just 1.0
        | isFinal b && fromJust (winner b) == 1 = Just 0.0
        | otherwise = Nothing

    -- Returns positions where the columns are not filled up.
    actions b = mapMaybe selectEmpty (zip [0..] (head (grid b))) where
        selectEmpty (i, p) = if p == EmptyPiece then Just (Drop i) else Nothing

    -- Applies action a to board b.
    apply (Drop p) b = b

dropPiece :: Int -> Board -> Board
dropPiece col board = board
    { grid = dropAt (PlayerPiece (turn board)) col (grid board)
    , turn = (turn board + 1) `mod` 2
    }

dropAt :: Piece -> Int -> [[Piece]] -> [[Piece]]
dropAt piece col gr =
    init emptyRows ++ [lrow ++ [piece] ++ rrow] ++ filledRows
    where
        (lrow, _:rrow) = splitAt col (last emptyRows)
        (emptyRows, filledRows) = span (\p -> (p !! col) == EmptyPiece) gr
