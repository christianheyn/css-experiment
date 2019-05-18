{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (
      toLine
    , tokenize) where

    import qualified Data.Text.Lazy as L
    import Types
    import Data.List (isPrefixOf)

    rmWhile :: (Char -> Bool) -> L.Text -> L.Text
    rmWhile fn s = if fn x then rmWhile fn xs else s
            where x = L.head s
                  xs = L.tail s

    toLine :: L.Text -> Line
    toLine xs
        | isPrefix "require "   = construc Require
        | isPrefix "include "   = construc Include
        | isPrefix "//"         = construc Comment
        | isPrefix "/*"         = construc CSSComment
        | isPrefix "function "  = construc FunctionStart
        | isPrefix "component " = construc ComponentStart
        | isPrefix "let "       = construc LetDeclaration
        | isPrefix "const "     = construc ConstDeclaration
        | otherwise             = construc CodeLine
        where rest = rmWhile (== ' ') xs
              nested = L.length xs - L.length rest
              isPrefix = (`L.isPrefixOf` rest)
              construc x = x nested rest

    tokenize = id
