module Barc.ExpBit (Prog(..), Exp(..)) where


data Prog = Prog { progWidth :: !Int
                 , progHeight :: !Int
                 , progExp :: !Exp
                 }
          deriving (Show, Eq)

data Exp = BitValue !Bool
         | BoardBitValue !Int !Int
         | Nand !Exp !Exp
         deriving (Show, Eq)
