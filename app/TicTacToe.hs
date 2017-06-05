module Main (main) where

import           Data.Maybe             (fromJust, isJust)
import           Game.Fishmax           (actions, apply, bestAction, emptyNode,
                                         timedMCTS)
import           Game.Fishmax.TicTacToe (Draw (..), State, start, winner)
import           System.IO              (hFlush, stdout)
import           System.Random          (getStdGen)

main :: IO ()
main = continue start

-- FIXME: Fix Tie Check
continue :: State -> IO ()
continue state = do
    -- Run MCTS for 5 seconds.
    rand      <- getStdGen
    finalNode <- timedMCTS 5 rand state emptyNode

    -- Pick the move with the best outcome.
    let computerAction = fst (bestAction finalNode)
    let computerState = apply computerAction state

    -- Put computer move and leave if it's a winning move.
    putStrLn ("Computer Chose: " ++ show computerAction)
    if   isJust (winner computerState)
    then putStrLn "Computer Wins"
    else
        if null (actions computerState)
        then putStrLn "It's a Tie!"
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
