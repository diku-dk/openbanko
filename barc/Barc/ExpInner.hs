module Barc.ExpInner (Prog(..), Exp(..), ExpList(..), ExpInt(..)) where


data Prog = Prog { progWidth :: Int
                 , progHeight :: Int
                 , progExp :: Exp
                 }
          deriving (Show, Eq)

data Exp = ListExp ExpList
         | IntExp ExpInt
         deriving (Show, Eq)

data ExpList = BoardXs
             | BoardYs
             | BoardValues
             | List [ExpInt]
             | Map { mapId :: Int
                   , mapLength :: ExpInt
                   , mapBody :: ExpInt
                   }
             deriving (Show, Eq)

data ExpInt = BoardWidth
            | BoardHeight
            | Index ExpList ExpInt
            | Const Int
            | CurrentIndex Int
            | Length ExpList
            | Reduce { reduceId :: Int
                     , reduceList :: ExpList
                     , reduceNeutral :: ExpInt
                     , reduceBody :: ExpInt
                     }
            | ReduceValueFirst Int
            | ReduceValueSecond Int
            | Nand ExpInt ExpInt
            | Add ExpInt ExpInt
            | Subtract ExpInt ExpInt
            | Multiply ExpInt ExpInt
            | Modulo ExpInt ExpInt
            | Eq ExpInt ExpInt
            | Gt ExpInt ExpInt
            deriving (Show, Eq)
