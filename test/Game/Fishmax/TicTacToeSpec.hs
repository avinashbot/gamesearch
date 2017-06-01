module Game.Fishmax.TicTacToeSpec (spec) where

import Test.Hspec
import Game.Fishmax.TicTacToe

spec :: Spec
spec = do
    describe "(+)" $ do
        it "adds numbers" $ do
            1 + 1 `shouldBe` 2
