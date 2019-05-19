{-# LANGUAGE OverloadedStrings #-}

module TokenizerSpec (spec) where

import Test.Hspec
import qualified Data.Text.Lazy as L
import Types
import Tokenizer (
      toLine
    , toLines
    , tokenize
    , toBlocks)

spec :: Spec
spec = do
    describe "toLine" $ do
        it "gives Line type Comment" $ do
            let inputStr = "    // comment"
                actual = toLine inputStr
                expected = Line { lineType = Comment, lineIndent = 4, lineContent = "// comment" }
            actual `shouldBe` expected

        it "gives Line type CSSComment" $ do
            let inputStr = "    /* css comment"
                actual = toLine inputStr
                expected = Line { lineType = CSSComment, lineIndent = 4, lineContent = "/* css comment" }
            actual `shouldBe` expected

        it "gives Line type Require" $ do
            let inputStr = "    require \"./file.hcss\""
                actual = toLine inputStr
                expected = Line { lineType = Require, lineIndent = 4, lineContent = "require \"./file.hcss\"" }
            actual `shouldBe` expected

        it "gives Line type Include" $ do
            let inputStr = "    include \"./file.hcss\""
                actual = toLine inputStr
                expected = Line { lineType = Include, lineIndent = 4, lineContent = "include \"./file.hcss\"" }
            actual `shouldBe` expected

        it "gives Line type FunctionStart" $ do
            let inputStr = "    function abc()"
                actual = toLine inputStr
                expected = Line { lineType = FunctionStart, lineIndent = 4, lineContent = "function abc()" }
            actual `shouldBe` expected

        it "gives Line type LetDeclaration" $ do
            let inputStr = "   let a ="
                actual = toLine inputStr
                expected = Line { lineType = LetDeclaration, lineIndent = 3, lineContent = "let a =" }
            actual `shouldBe` expected

        it "gives Line type ConstDeclaration" $ do
            let inputStr = "   const a ="
                actual = toLine inputStr
                expected = Line { lineType = ConstDeclaration, lineIndent = 3, lineContent = "const a =" }
            actual `shouldBe` expected

        it "gives Line type ComponentStart" $ do
            let inputStr = "   component a"
                actual = toLine inputStr
                expected = Line { lineType = ComponentStart, lineIndent = 3, lineContent = "component a" }
            actual `shouldBe` expected

        it "nested level can be 0" $ do
            let inputStr = "<section>"
                actual = toLine inputStr
                expected = Line { lineType = CodeLine, lineIndent = 0, lineContent = "<section>" }
            actual `shouldBe` expected

    describe "toLines" $ do
        it "works with empty input" $ do
            let inputStr = ""
                actual = toLines inputStr
                expected = [] :: [Line]
            actual `shouldBe` expected

        it "splits input text to Line" $ do
            let inputStr = " code\n  require"
                actual = toLines inputStr
                expected = [
                      Line { lineType = CodeLine, lineIndent = 1, lineContent = "code" },
                      Line { lineType = CodeLine, lineIndent = 2, lineContent = "require" }
                    ]
            actual `shouldBe` expected

    describe "toBlocks" $ do
        it "works with empty input" $ do
            let inputStr = ""
                actual = toBlocks $ toLines inputStr
                expected = [] :: [Block]
            actual `shouldBe` expected

        it "crteates nested Blocks" $ do
            let inputStr = "level 1.0\nlevel 1.1\n   level 2.0\n   level 2.1\n     level 3.0"
                actual = toBlocks $ toLines inputStr
                expected = [] :: [Block]
            actual `shouldBe` expected
