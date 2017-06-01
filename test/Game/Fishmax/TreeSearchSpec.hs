module Game.Fishmax.TreeSearchSpec (spec) where

import Test.Hspec
import Game.Fishmax.TreeSearch (monteCarlo, emptyNode, payouts, apply)
import Game.Fishmax.TicTacToe (start, PlaceSymbol(..))
import System.Random (mkStdGen)

spec :: Spec
spec = do
    describe "strip" $ do
        it "removes leading and trailing whitespace" $ do
            putStrLn $ show $ payouts $ fst $ hey 10000

hey 0 = monteCarlo (mkStdGen 20) (apply (PlaceSymbol (2,2)) start) emptyNode
hey i = monteCarlo pr start pn where (pn, (pr, _)) = hey (i - 1)
