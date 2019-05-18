{-# LANGUAGE OverloadedStrings #-}

module TokenizerSpec (spec) where

import Test.Hspec
import qualified Data.Text.Lazy as L
import Types
import Tokenizer (toLine ,tokenize)

spec :: Spec
spec = do
    describe "toLine" $ do
        it "gives Line type Comment" $ do
            let inputStr = "    // comment"
                actual = toLine inputStr
                expected = Comment 4 "// comment"
            actual `shouldBe` expected

        it "gives Line type CSSComment" $ do
            let inputStr = "    /* css comment"
                actual = toLine inputStr
                expected = CSSComment 4 "/* css comment"
            actual `shouldBe` expected

        it "gives Line type Require" $ do
            let inputStr = "    require \"./file.hcss\""
                actual = toLine inputStr
                expected = Require 4 "require \"./file.hcss\""
            actual `shouldBe` expected

        it "gives Line type Include" $ do
            let inputStr = "    include \"./file.hcss\""
                actual = toLine inputStr
                expected = Include 4 "include \"./file.hcss\""
            actual `shouldBe` expected

        it "gives Line type FunctionStart" $ do
            let inputStr = "    function abc()"
                actual = toLine inputStr
                expected = FunctionStart 4 "function abc()"
            actual `shouldBe` expected

        it "gives Line type LetDeclaration" $ do
            let inputStr = "   let a ="
                actual = toLine inputStr
                expected = LetDeclaration 3 "let a ="
            actual `shouldBe` expected

        it "gives Line type ConstDeclaration" $ do
            let inputStr = "   const a ="
                actual = toLine inputStr
                expected = ConstDeclaration 3 "const a ="
            actual `shouldBe` expected

        it "gives Line type ComponentStart" $ do
            let inputStr = "   component a"
                actual = toLine inputStr
                expected = ComponentStart 3 "component a"
            actual `shouldBe` expected

        it "nested level can be 0" $ do
            let inputStr = "<section>"
                actual = toLine inputStr
                expected = CodeLine 0 "<section>"
            actual `shouldBe` expected
