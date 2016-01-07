module Barc.CodeGen (genCode) where

import Barc.ExpUnrolled


-- int8_t hardcoded for now
genCode :: Prog -> String
genCode (Prog w h e) = "#include <stdlib.h>\n"
                       ++ "#include <stdio.h>\n"
                       ++ "#include <stdint.h>\n"
                       ++ "\n"
                       ++ "int board_is_okay(int8_t* vs) {\n"
                       ++ "return (" ++ codeExp e ++ ");\n"
                       ++ "}"
                       ++ "\n"
                       ++ "int main() {\n"
                       ++ "int8_t vs[" ++ len ++ "];\n"
                       ++ scanfs
                       ++ "int okay = board_is_okay(vs);\n"
                       ++ "return (okay ? 0 : 1);\n"
                       ++ "}\n"
  where len = show (w * h)
        scanfs = concatMap (\i -> "scanf(\"%hhd\", &vs[" ++ show i ++ "]);\n") [0..w * h - 1]

codeExp :: Exp -> String
codeExp = (++ "\n") . codeExp'

codeExp' :: Exp -> String
codeExp' e = case e of
  BoardValue i -> "vs[" ++ show i ++ "]"
  Const n -> show n
  Nand e0 e1 -> "(1 ^ ((" ++ codeExp e0 ++ " > 0) & (" ++ codeExp e1 ++ " > 0)))"
  Add e0 e1 -> codeArith "+" e0 e1
  Subtract e0 e1 -> codeArith "-" e0 e1
  Multiply e0 e1 -> codeArith "*" e0 e1
  Modulo e0 e1 -> codeArith "%" e0 e1
  Eq e0 e1 -> codeArith "==" e0 e1
  Gt e0 e1 -> codeArith ">" e0 e1

codeArith :: String -> Exp -> Exp -> String
codeArith op e0 e1 = "(" ++ codeExp e0 ++ " " ++ op ++ " " ++ codeExp e1 ++ ")"
