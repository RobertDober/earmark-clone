import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
    describe "Tests" $ do
        it "returns true" $ do
            True `shouldBe` True

        it "has access to InputLine" $ do
            putStrLn $ show $ Lib.InputLine "a" 42
