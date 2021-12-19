import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Tests" $ do
        it "returns true" $ do
            True `shouldBe` True
