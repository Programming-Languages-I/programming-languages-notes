module AST ( Statement (..)
           , Identifier
           , Expression
           , Term
           , Factor
           ) where

data Statement = Assign String Expression
               deriving (Show, Eq)

data Expression = Plus Term Term
                | Minus Term Term 
                | Term
                deriving (Show, Eq)

data Term = Div Factor Factor
          | Mul Factor Factor
          | Factor
          deriving (Show, Eq)

data Factor = Value Int deriving (Show, Eq)           

type Identifier = String
