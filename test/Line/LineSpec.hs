module Line.LineSpec
    (spec
    ) where

import Test.Hspec

import Line

spec :: Spec
spec = do
    describe "Line" $ do
        it "can be constructed" $ do
            putStrLn $ show $ Text LineInfo{text="Hello", lnb=0, indent=0, content="Hello"}
