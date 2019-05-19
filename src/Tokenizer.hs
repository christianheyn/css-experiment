{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (
      toLine
    , toLines
    , tokenize
    , toBlocks
    , prettyShowAll) where

    import qualified Data.Text.Lazy as L
    import qualified Data.Int as I
    import Types
    import Data.List (isPrefixOf)

    rmWhile :: (Char -> Bool) -> L.Text -> L.Text
    rmWhile fn s = if fn x then rmWhile fn xs else s
            where x = L.head s
                  xs = L.tail s

    toLine :: L.Text -> Line
    toLine xs
        | isPrefix "require "   = withLineType Require
        | isPrefix "include "   = withLineType Include
        | isPrefix "//"         = withLineType Comment
        | isPrefix "/*"         = withLineType CSSComment
        | isPrefix "function "  = withLineType FunctionStart
        | isPrefix "component " = withLineType ComponentStart
        | isPrefix "let "       = withLineType LetDeclaration
        | isPrefix "const "     = withLineType ConstDeclaration
        | otherwise             = withLineType CodeLine
        where rest = rmWhile (== ' ') xs
              nested = L.length xs - L.length rest
              isPrefix = (`L.isPrefixOf` rest)
              withLineType x = Line {
                                    lineType = x,
                                    lineIndent = nested,
                                    lineContent = rest }

    toLines :: L.Text -> [Line]
    toLines input = map toLine ls
        where ls = L.lines input

    higherIndent a b = (lineIndent a) < (lineIndent b)

    toBlocks :: [Line] -> [Block]
    toBlocks []     = []
    toBlocks all@(l:ls) = [newBlock] ++ (toBlocks next)
        where newBlock = Block l (toBlocks $ inner)
              inner = takeWhile (higherIndent l) ls
              next = dropWhile (higherIndent l) ls

    tokenize = id
