module Game.Fishmax.ConnectFourSpec (spec) where

import Test.Hspec
import System.Random (getStdGen)
import Game.Fishmax (monteCarlo, emptyNode, best, apply)
import Game.Fishmax.ConnectFour (start, State, Drop(..))

spec :: Spec
spec = do
    describe "monteCarlo + connectFour" $ do
        it "works as expected" $ do
            rand <- getStdGen
            print $ best $ fst $ hey rand 500000

startAc = foldl (flip (apply . Drop)) start []

hey r 0 = monteCarlo r startAc emptyNode
hey r i = monteCarlo pr startAc pn where (pn, (pr, _)) = hey r (i - 1)
