{-# LANGUAGE OverloadedStrings #-}
module Barc.CCodeGen (genCode) where

import Barc.ExpUnrolled

import qualified Data.Text as T


-- int8_t hardcoded for now

genCode :: Prog -> T.Text
genCode (Prog w h e) =
  T.concat [ "#include <stdlib.h>\n"
           , "#include <stdio.h>\n"
           , "#include <stdint.h>\n"
           , "\n"
           , "int board_is_okay(int8_t* vs) {\n"
           , "return ("
           , codeExp e
           , ");\n"
           , "}"
           , "\n"
           , "int main() {\n"
           , "int8_t vs["
           , T.pack len
           , "];\n"
           , scanfs
           , "int okay = board_is_okay(vs);\n"
           , "return (okay ? 0 : 1);\n"
           , "}\n"
           ]
  where len = show (w * h)
        scanfs = T.concat $
                 map (\i -> T.concat [ "scanf(\"%hhd\", &vs["
                                     , T.pack (show i)
                                     , "]);\n"
                                     ])
                 [0..w * h - 1]

codeExp :: Exp -> T.Text
codeExp e = T.append (codeExp' e) "\n"

codeExp' :: Exp -> T.Text
codeExp' e = case e of
  BoardValue i -> T.concat [ "vs["
                           , T.pack (show i)
                           , "]"
                           ]
  Const n -> T.pack (show n)
  Nand e0 e1 -> T.concat [ "(1 ^ (("
                         , codeExp e0
                         , " > 0) & ("
                         , codeExp e1
                         , " > 0)))"
                         ]
  Add e0 e1 -> codeArith "+" e0 e1
  Subtract e0 e1 -> codeArith "-" e0 e1
  Multiply e0 e1 -> codeArith "*" e0 e1
  Modulo e0 e1 -> codeArith "%" e0 e1
  Eq e0 e1 -> codeArith "==" e0 e1
  Gt e0 e1 -> codeArith ">" e0 e1

codeArith :: T.Text -> Exp -> Exp -> T.Text
codeArith op e0 e1 = T.concat [ "("
                              , codeExp e0
                              , " "
                              , op
                              , " "
                              , codeExp e1
                              , ")"
                              ]
