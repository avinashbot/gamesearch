{-# LANGUAGE BangPatterns #-}

module Game.Fishmax
  ( module Game.Fishmax.TreeSearch
  , timedMCTS
  ) where

import Game.Fishmax.TreeSearch
import System.CPUTime (getCPUTime)
import System.Random (RandomGen)

-- Run monte carlo search for a given number of seconds.
timedMCTS ::
     (RandomGen r, Spec s a) => Integer -> r -> s -> Node a -> IO (Node a)
timedMCTS seconds r s n = do
  curTime <- getCPUTime
  timedMCTS' (curTime + seconds * 1000000000000) r s n

-- Run monte carlo search until cpuTime hits a certain value.
timedMCTS' ::
     (RandomGen r, Spec s a) => Integer -> r -> s -> Node a -> IO (Node a)
timedMCTS' stopTime r s n = do
  let !(nn, (nr, _)) = monteCarlo r s n
  curTime <- getCPUTime
  if stopTime >= curTime
    then timedMCTS' stopTime nr s nn
    else return nn
