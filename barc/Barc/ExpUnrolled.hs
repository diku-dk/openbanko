module Barc.ExpUnrolled (Prog(..), Exp(..)) where


data Prog = Prog { progWidth :: Int
                 , progHeight :: Int
                 , progExp :: Exp
                 }
          deriving (Show, Eq)

data Exp = BoardValue Int
         | Const Int
         | Nand Exp Exp
         | Add Exp Exp
         | Subtract Exp Exp
         | Multiply Exp Exp
         | Modulo Exp Exp
         | Eq Exp Exp
         | Gt Exp Exp
         deriving (Show, Eq)
