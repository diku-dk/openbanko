module Barc.ExpUnrolled (Prog(..), Exp(..),
                         ExpInt(..), ExpBool(..)) where


data Prog = Prog { progWidth :: Int
                 , progHeight :: Int
                 , progExp :: ExpBool
                 }
          deriving (Show, Eq)

data Exp = IntExp ExpInt
         | BoolExp ExpBool
         deriving (Show, Eq)

data ExpInt = BoardValue Int
            | Const Int
            | IntConv ExpBool
            | Add ExpInt ExpInt
            | Subtract ExpInt ExpInt
            | Multiply ExpInt ExpInt
            | Modulo ExpInt ExpInt
            deriving (Show, Eq)

data ExpBool = BoolVal Bool
             | BoolConv ExpInt
             | And ExpBool ExpBool
             | Or ExpBool ExpBool
             | Not ExpBool
             | Eq ExpInt ExpInt
             | Gt ExpInt ExpInt
             deriving (Show, Eq)
