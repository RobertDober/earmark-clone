module Line.SpaceSpec
    (spec
    ) where

import Test.Hspec
import Line

spec :: Spec
spec = do
    describe "Blank" $ do
        it "can be empty" $ do
            let expected = Blank (LineInfo {text="", lnb=1, indent=0, content=""})
                in
                scanLine ("", 1) `shouldBe` expected

        it "or have spaces" $ do
            let expected = Blank (LineInfo {text="  ", lnb=2, indent=2, content=""})
                in
                scanLine ("  ", 2) `shouldBe` expected

        it "blanks never yield Indent" $ do
            let expected = Blank (LineInfo {text="    ", lnb=3, indent=4, content=""})
                in
                scanLine ("    ", 3) `shouldBe` expected

    describe "Indent" $ do
        it "needs at least 4 spaces" $ do
            let expected = Indent (LineInfo {text="    *", lnb=4, indent=4, content="*"})
                in
                scanLine ("    *", 4) `shouldBe` expected
