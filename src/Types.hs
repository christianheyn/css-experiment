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

class PrettyShow a where
    prettyShow :: a -> String
    prettyShowAll :: [a] -> String

prettyShow' i (Block l bs) = ind ++ (L.unpack $ lineContent l) ++ "\n" ++ children
    where ind = (take (i * 4) (repeat ' '))
          children = concat $ map (prettyShow' (i + 1)) bs

instance PrettyShow Block where
    prettyShow = prettyShow' 0
    prettyShowAll bs = concat $ map (prettyShow' 0) bs
