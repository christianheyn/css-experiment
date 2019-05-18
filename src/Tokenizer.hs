{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (
      toLine
    , tokenize) where

    import Types
    import Data.List (isPrefixOf)

    rmWhile fn l@(x:xs) = if fn x then rmWhile fn xs else l

    toLine :: String -> Line
    toLine xs
        | isPrefix "require "   = construc Require
        | isPrefix "include "   = construc Include
        | isPrefix "//"         = construc Comment
        | isPrefix "/*"         = construc CSSComment
        | isPrefix "function "  = construc FunctionStart
        | isPrefix "component " = construc ComponentStart
        | isPrefix "let "       = construc LetDeclaration
        | isPrefix "const "     = construc ConstDeclaration
        | otherwise = CodeLine nested rest
        where rest = rmWhile (== ' ') xs
              nested = length xs - length rest
              isPrefix = (`isPrefixOf` rest)
              construc x = x nested rest

    tokenize = id
