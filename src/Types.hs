{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text.Lazy as L
import qualified Data.Int as I

type NestingLevel = I.Int64
type LeftTrimmed = L.Text

data LineType =
      CodeLine
    | Require
    | Include
    | Comment
    | CSSComment
    | FunctionStart
    | ComponentStart
    | LetDeclaration
    | ConstDeclaration
    deriving (Show, Eq)

data Line = Line {
    lineIndent  :: NestingLevel,
    lineContent :: LeftTrimmed,
    lineType    :: LineType
    } deriving (Show, Eq)

data Block = Block Line [Block] deriving (Show, Eq)
