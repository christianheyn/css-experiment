{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (
      toLine
    , tokenize) where

    import Types
    import Data.List (isPrefixOf)

    rmWhile fn l@(x:xs) = if fn x then rmWhile fn xs else l

    toLine :: String -> Line
    toLine xs
        | "require "  `isPrefixOf` rest = Require nested rest
        | "//"        `isPrefixOf` rest = Comment nested rest
        | "/*"        `isPrefixOf` rest = CSSComment nested rest
        | "template " `isPrefixOf` rest = TemplateStart nested rest
        | "let "      `isPrefixOf` rest = LetDeclaration nested rest
        | "const "    `isPrefixOf` rest = ConstDeclaration nested rest
        | otherwise = CodeLine nested rest
        where rest = rmWhile (== ' ') xs
              nested = length xs - length rest

    tokenize = id
