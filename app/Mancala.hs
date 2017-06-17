module Main (main) where

import Game.GameSearch         (Spec (..), Node, child, bestAction, empty,
                                timedMCTS)
import Game.GameSearch.Mancala (State (..), Player (..), start)
import System.IO               (hFlush, stdout)
import System.Random           (getStdGen)

main :: IO ()
main = let newState = start 6 4 in print newState >> continue newState

playerof (State _ p) = p

continue :: State -> IO ()
continue state
    | playerof state == Max = do
        -- Run MCTS for 2 seconds.
        rand      <- getStdGen
        finalNode <- timedMCTS 5 rand state empty

        -- Pick the move with the best outcome.
        let action = bestAction finalNode state
        let nextState = apply action state
        putStrLn ("Computer Chose: " ++ show action)
        print nextState
        if null (actions nextState)
            then putStrLn "End of Game!"
            else continue nextState
    | playerof state == Min = do
        -- Ask player for their move.
        putStr "Player Move (0..5): "
        hFlush stdout
        uinput <- getLine

        let action = read uinput + 7
        let nextState = apply action state
        print nextState
        if null (actions nextState)
            then putStrLn "End of Game!"
            else continue nextState
