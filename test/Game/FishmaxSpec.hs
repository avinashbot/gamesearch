module Game.FishmaxSpec (spec) where

import Test.Hspec
import System.Random (mkStdGen)
import Game.Fishmax (monteCarlo, emptyNode, payouts, apply)
import Game.Fishmax.TicTacToe (start, Draw(..))

spec :: Spec
spec = do
    describe "monteCarlo" $ do
        it "works as expected" $ do
            print $ payouts $ fst $ hey 255168

startAc = apply (Draw (1,1)) start

hey 0 = monteCarlo (mkStdGen 20) startAc emptyNode
hey i = monteCarlo pr startAc pn where (pn, (pr, _)) = hey (i - 1)
