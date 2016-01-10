module Barc.ExpInner (Prog(..), Exp(..),
                      ExpIntList(..), ExpBoolList(..),
                      ExpInt(..), ExpBool(..)) where


data Prog = Prog { progWidth :: Int
                 , progHeight :: Int
                 , progExp :: ExpBool
                 }
          deriving (Show, Eq)

data Exp = IntListExp ExpIntList
         | BoolListExp ExpBoolList
         | IntExp ExpInt
         | BoolExp ExpBool
         deriving (Show, Eq)

data ExpIntList = BoardValues
                | ListInt [ExpInt]
                | MapInt { mapIntId :: Int
                         , mapIntLength :: ExpInt
                         , mapIntBody :: ExpInt
                         }
                deriving (Show, Eq)

data ExpBoolList = ListBool [ExpBool]
                 | MapBool { mapBoolId :: Int
                           , mapBoolLength :: ExpInt
                           , mapBoolBody :: ExpBool
                           }
                 deriving (Show, Eq)

data ExpInt = Const Int
            | IndexInt ExpIntList ExpInt
            | CurrentIndex Int
            | LengthInt ExpIntList
            | LengthBool ExpBoolList
            | IntConv ExpBool
            | ReduceInt { reduceIntId :: Int
                        , reduceIntNeutral :: ExpInt
                        , reduceIntList :: ExpIntList
                        , reduceIntBody :: ExpInt
                        }
            | ReduceIntValueFirst Int
            | ReduceIntValueSecond Int
            | Add ExpInt ExpInt
            | Subtract ExpInt ExpInt
            | Multiply ExpInt ExpInt
            | Modulo ExpInt ExpInt
            deriving (Show, Eq)

data ExpBool = BoolVal Bool
             | IndexBool ExpBoolList ExpInt
             | BoolConv ExpInt
             | ReduceBool { reduceBoolId :: Int
                          , reduceBoolNeutral :: ExpBool
                          , reduceBoolList :: ExpBoolList
                          , reduceBoolBody :: ExpBool
                          }
             | ReduceBoolValueFirst Int
             | ReduceBoolValueSecond Int
             | And ExpBool ExpBool
             | Or ExpBool ExpBool
             | Not ExpBool
             | Eq ExpInt ExpInt
             | Gt ExpInt ExpInt
             deriving (Show, Eq)
