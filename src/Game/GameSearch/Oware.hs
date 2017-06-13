{-# LANGUAGE MultiParamTypeClasses #-}

module Game.GameSearch.Oware
    ( Board(..)
    , Action(..)
    , Player(..)
    , start
    ) where

import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Game.GameSearch (Spec(..))

data Player
    = Max
    | Min
    deriving (Eq, Ord, Show)

data Winner
    = Definite Player
    | Tie
    deriving (Eq)

newtype Action =
    Action Int
    deriving (Eq, Ord, Show)

data Board = Board
    { grid :: Seq.Seq Int
    , scores :: (Double, Double)
    , turn :: Player
    }

instance Show Board where
    show board =
        "Min -> " ++
        foldl
            (\sm ix ->
                 sm ++ "[" ++ show (grid board `Seq.index` (17 - ix)) ++ "] ")
            ""
            [6 .. 11] ++
        "\nMax -> " ++
        foldl
            (\sm ix -> sm ++ "[" ++ show (grid board `Seq.index` ix) ++ "] ")
            ""
            [0 .. 5]

instance Spec Board Action Player where
    actions = validActions
    player = turn
    apply a b =
        (sow a b)
        { turn =
              if turn b == Max
                  then Min
                  else Max
        }
    -- Returns a payout of 1 if we won, 0 if we lost.
    payouts b
        | winner b == Just (Definite Max) = [(Max, fst (scores b)), (Min, -(fst (scores b)))]
        | winner b == Just (Definite Min) = [(Max, -(snd (scores b))), (Min, snd (scores b))]
        | winner b == Just Tie = [(Max, 0.0), (Min, 0.0)]
        | isNothing (winner b) && null (actions b) = [(Max, 0.0), (Min, 0.0)]

start :: Board
start = Board {grid = Seq.replicate 12 4, scores = (0, 0), turn = Max}

-- TODO: remove actions that would starve the opponent.
validActions :: Board -> [(Double, Action)]
validActions board =
    map (\a -> (1, Action a)) $
    filter (\i -> (grid board `Seq.index` i) > 0) $
    if turn board == Max
        then [0 .. 5]
        else [6 .. 11]

-- Start sowing seeds from a pot.
sow :: Action -> Board -> Board
sow (Action pos) board =
    fst $
    propagate
        (grid board `Seq.index` pos)
        (next pos)
        board {grid = Seq.update pos 0 (grid board)}

-- Deposit a number of stones from a certain position on the board.
propagate :: Int -> Int -> Board -> (Board, Bool)
propagate 0 _ board = (board, True)
propagate i pos board'
    | oppHome pos board && canScore (grid board `Seq.index` pos) && canCapture =
        ( board
          { grid = Seq.update pos 0 (grid board)
          , scores = addScore ((grid board `Seq.index` pos) + 1) board
          }
        , True)
    | otherwise = (board {grid = Seq.adjust (+ 1) pos (grid board)}, False)
  where
    (board, canCapture) = propagate (i - 1) (next pos) board'

-- Add pebbles to the current player of the board.
addScore :: Int -> Board -> (Double, Double)
addScore num board =
    case turn board of
        Max -> (fst (scores board) + fromIntegral num, snd (scores board))
        Min -> (fst (scores board), snd (scores board) + fromIntegral num)

-- Whether the position can be scored on.
oppHome :: Int -> Board -> Bool
oppHome pos board =
    case turn board of
        Max -> pos >= 6 && pos < 12
        Min -> pos >= 0 && pos < 6

-- Whether the count, when added, can lead to capture.
canScore :: Int -> Bool
canScore count = count == 1 || count == 2

-- The next index.
next :: Int -> Int
next 11 = 0
next i = i + 1

winner :: Board -> Maybe Winner
winner board
    | fst (scores board) > 24 = Just (Definite Max)
    | snd (scores board) > 24 = Just (Definite Min)
    | fst (scores board) == 24 && snd (scores board) == 24 = Just Tie
    | otherwise = Nothing
