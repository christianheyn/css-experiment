{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text.Lazy as L
import qualified Data.Int as I

type NestingLevel = I.Int64
type LeftTrimmed = L.Text

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
