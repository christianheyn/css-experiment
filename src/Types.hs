{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text.Lazy as L

type NestingLevel = Int
type LeftTrimmed = String

data Line =
      CodeLine         NestingLevel LeftTrimmed
    | Require          NestingLevel LeftTrimmed
    | Include          NestingLevel LeftTrimmed
    | Comment          NestingLevel LeftTrimmed
    | CSSComment       NestingLevel LeftTrimmed
    | FunctionStart    NestingLevel LeftTrimmed
    | ComponentStart   NestingLevel LeftTrimmed
    | LetDeclaration   NestingLevel LeftTrimmed
    | ConstDeclaration NestingLevel LeftTrimmed
    | FileStart        NestingLevel FilePath
    deriving (Show, Eq)

data Block = Block Line [Block]
