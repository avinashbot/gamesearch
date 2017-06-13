module Main (main) where

import Game.GameSearch           (Spec (..), bestAction, empty, timedMCTS)
import Game.GameSearch.Oware     (Board (..), Action (..), start)
import System.IO                 (hFlush, stdout)
import System.Random             (getStdGen)

main :: IO ()
main = continue start

continue :: Board -> IO ()
continue state = do
    print state

    -- Run MCTS for 2 seconds.
    rand      <- getStdGen
    finalNode <- timedMCTS 5 rand state empty

    -- Pick the move with the best outcome.
    let computerAction = bestAction finalNode state
    let computerBoard = apply computerAction state
    putStrLn ("Computer Chose: " ++ show computerAction)

    -- Ask player for their move.
    putStr "Player Move (6..11): "
    hFlush stdout
    uinput <- getLine

    -- Check if it's a winning move, else go back.
    let playerBoard = apply (Action (read uinput)) computerBoard
    continue playerBoard
