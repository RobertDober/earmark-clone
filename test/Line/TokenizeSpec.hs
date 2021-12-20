module Line.TokenizeSpec
    (spec
    ) where

import Test.Hspec
import Line

spec :: Spec
spec = do
    describe "tokenize just text" $ do
        it "creates a Text" $ do
            let expected = Text LineInfo {text="hello", lnb=42, indent=0, content="hello"}
                in
                    scanLine ("hello", 42) `shouldBe` expected
        it "creates a Text with indent" $ do
            let expected = Text LineInfo {text=" hello", lnb=43, indent=1, content="hello"}
                in
                    scanLine (" hello", 43) `shouldBe` expected
