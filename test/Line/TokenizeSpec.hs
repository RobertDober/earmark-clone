module Line.TokenizeSpec
    (spec
    ) where

import Test.Hspec
import Line

spec :: Spec
spec = do
    describe "tokenize just text" $ do
        it "creates a Text" $ do
            let expected = Text (LineInfo {text="hello", lnb=42, indent=0, content="hello"})
                in
                    scanLine ("hello", 42) `shouldBe` expected
        it "text with one leading space" $ do
            let expected = Text (LineInfo {text=" hello", lnb=43, indent=1, content="hello"})
                in
                    scanLine (" hello", 43) `shouldBe` expected

    describe "tokenize ruler" $ do
        it "can be dashes" $ do
            let expected = Ruler (LineInfo {text="---", lnb=44, indent=0, content="---"}) ThinRuler
                in
                    scanLine ("---", 44) `shouldBe` expected

        it "but three dashes are needed" $ do
            let expected = Text (LineInfo {text="--", lnb=44, indent=0, content="--"})
                in
                    scanLine ("--", 44) `shouldBe` expected

        it "can be underscores" $ do
            let expected = Ruler (LineInfo {text="___", lnb=45, indent=0, content="___"}) MediumRuler
                in
                    scanLine ("___", 45) `shouldBe` expected

        it "Thick Indented Ruler" $ do
            let expected = Ruler (LineInfo {text="   ***", lnb=46, indent=3, content="***"}) ThickRuler
                in
                    scanLine ("   ***", 46) `shouldBe` expected

