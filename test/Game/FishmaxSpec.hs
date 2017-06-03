module Game.FishmaxSpec (spec) where

import Test.Hspec
import System.Random (getStdGen)
import Game.Fishmax (monteCarlo, emptyNode, payouts, apply)
import Game.Fishmax.TicTacToe (start, Draw(..))

spec :: Spec
spec = do
    describe "(+)" $ do
        it "works as expected" $ do
            1 + 1 `shouldBe` 2
