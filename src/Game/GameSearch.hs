{-# LANGUAGE BangPatterns #-}

module Game.GameSearch
  ( module Game.GameSearch.TreeSearch
  , timedMCTS
  ) where

import Game.GameSearch.TreeSearch
import System.CPUTime (getCPUTime)
import System.Random (StdGen)

-- Run monte carlo search for a given number of seconds.
timedMCTS ::
     Spec s a p => Integer -> StdGen -> s -> Node a p -> IO (Node a p)
timedMCTS seconds r s n = do
  curTime <- getCPUTime
  timedMCTS' (curTime + seconds * 1000000000000) r s n

-- Run monte carlo search until cpuTime hits a certain value.
timedMCTS' ::
     Spec s a p => Integer -> StdGen -> s -> Node a p -> IO (Node a p)
timedMCTS' stopTime r s n = do
  let !(nr, nn) = monteCarlo r s n
  curTime <- getCPUTime
  if stopTime >= curTime
    then timedMCTS' stopTime nr s nn
    else return nn
