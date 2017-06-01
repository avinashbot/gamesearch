module Game.FishmaxSpec (spec) where

import Test.Hspec
import System.Random (getStdGen)
import Game.Fishmax (monteCarlo, emptyNode, payouts, apply)
import Game.Fishmax.TicTacToe (start, Draw(..))

spec :: Spec
spec = do
    describe "monteCarlo" $ do
        it "works as expected" $ do
            rand <- getStdGen
            print $ payouts $ fst $ hey rand 10000

startAc = apply (Draw (1,1)) start

hey r 0 = monteCarlo r startAc emptyNode
hey r i = monteCarlo pr startAc pn where (pn, (pr, _)) = hey r (i - 1)
