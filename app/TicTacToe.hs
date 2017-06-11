module Main (main) where

import Data.Maybe                (fromJust, isJust)
import Game.GameSearch           (Spec (..), bestAction, empty, timedMCTS)
import Game.GameSearch.TicTacToe (Draw (..), State(..), Player, start, winner)
import System.IO                 (hFlush, stdout)
import System.Random             (getStdGen)

main :: IO ()
main = continue start

continue :: State -> IO ()
continue state = do
    -- Run MCTS for 2 seconds.
    rand      <- getStdGen
    finalNode <- timedMCTS 2 rand state empty

    -- Check for an inevitable tie.
    if   length (actions state) == 1
    then putStrLn "It's a Tie!"
    else do
        -- Pick the move with the best outcome.
        let computerAction = bestAction finalNode state
        let computerState = apply computerAction state
        putStrLn ("Computer Chose: " ++ show computerAction)
        if   isJust (winner computerState)
        then putStrLn "Computer Wins"
        else do
            -- Ask player for their move.
            putStr "Player Move '(X, Y)': "
            hFlush stdout
            uinput <- getLine

            -- Check if it's a winning move, else go back.
            let playerState = apply (Draw (read uinput)) computerState
            if   isJust (winner playerState)
            then putStrLn "Player Wins"
            else continue playerState
