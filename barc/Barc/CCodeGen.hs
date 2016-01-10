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
           , "int board_is_okay(int8_t vs["
           , len
           , "]) {\n"
           , "return ("
           , codeBoolExp e
           , ");\n"
           , "}"
           , "\n"
           , "int main() {\n"
           , "int8_t vs["
           , len
           , "];\n"
           , scanfs
           , "int okay = board_is_okay(vs);\n"
           , "return (okay ? 0 : 1);\n"
           , "}\n"
           ]
  where len = T.pack $ show (w * h)
        scanfs = T.concat $
                 map (\i -> T.concat [ "scanf(\"%hhd\", &vs["
                                     , T.pack (show i)
                                     , "]);\n"
                                     ])
                 [0..w * h - 1]

codeIntExp :: ExpInt -> T.Text
codeIntExp e = T.append (codeIntExp' e) "\n"

codeIntExp' :: ExpInt -> T.Text
codeIntExp' e = case e of
  BoardValue i -> T.concat [ "vs["
                           , T.pack (show i)
                           , "]"
                           ]
  Const n -> T.pack (show n)
  IntConv e' -> codeBoolExp' e'
  Add e0 e1 -> codeIntBinOp "+" e0 e1
  Subtract e0 e1 -> codeIntBinOp "-" e0 e1
  Multiply e0 e1 -> codeIntBinOp "*" e0 e1
  Modulo e0 e1 -> codeIntBinOp "%" e0 e1

codeIntBinOp :: T.Text -> ExpInt -> ExpInt -> T.Text
codeIntBinOp op e0 e1 = T.concat [ "("
                                 , codeIntExp e0
                                 , " "
                                 , op
                                 , " "
                                 , codeIntExp e1
                                 , ")"
                                 ]

codeBoolExp :: ExpBool -> T.Text
codeBoolExp e = T.append (codeBoolExp' e) "\n"

codeBoolExp' :: ExpBool -> T.Text
codeBoolExp' e = case e of
  BoolVal False -> "0"
  BoolVal True -> "1"
  BoolConv e' -> codeIntExp' e'
  And e0 e1 -> codeBoolBinOp "&" e0 e1
  Or e0 e1 -> codeBoolBinOp "|" e0 e1
  Not e' -> codeBoolBinOp "^" e' (BoolVal True)
  Eq e0 e1 -> codeBoolIntBinOp "==" e0 e1
  Gt e0 e1 -> codeBoolIntBinOp ">" e0 e1

codeBoolBinOp :: T.Text -> ExpBool -> ExpBool -> T.Text
codeBoolBinOp op e0 e1 = T.concat [ "("
                                  , codeBoolExp e0
                                  , " "
                                  , op
                                  , " "
                                  , codeBoolExp e1
                                  , ")"
                                  ]

codeBoolIntBinOp :: T.Text -> ExpInt -> ExpInt -> T.Text
codeBoolIntBinOp op e0 e1 = T.concat [ "("
                                     , codeIntExp e0
                                     , " "
                                     , op
                                     , " "
                                     , codeIntExp e1
                                     , ")"
                                     ]
