module Barc.ExpOuter (Prog(..), Exp(..), Fun(..)) where


data Prog = Prog { progWidth :: Int
                 , progHeight :: Int
                 , progExp :: Exp
                 }
          deriving (Show, Eq)

data Exp = BoardXs
         | BoardYs
         | BoardValues
         | BoardWidth
         | BoardHeight
         | Let String Exp Exp
         | Var String
         | Const Int
         | BoolVal Bool
         | List [Exp]
         | Index Exp Exp
         | Length Exp
         | IntConv Exp
         | BoolConv Exp
         | Seq Exp Exp
         | All Exp
         | Any Exp
         | Sum Exp
         | Product Exp
         | Map Fun Exp
         | Reduce Fun Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Add Exp Exp
         | Subtract Exp Exp
         | Multiply Exp Exp
         | Modulo Exp Exp
         | Eq Exp Exp
         | Gt Exp Exp
         | GtEq Exp Exp
         | Lt Exp Exp
         | LtEq Exp Exp
         deriving (Show, Eq)

data Fun = Fun [String] Exp
         deriving (Show, Eq)
