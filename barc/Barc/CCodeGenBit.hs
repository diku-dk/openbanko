{-# LANGUAGE OverloadedStrings #-}
module Barc.CCodeGenBit (genCodeBit) where

import Barc.ExpBit

import qualified Data.Text as T


-- int8_t hardcoded for now

genCodeBit :: Prog -> T.Text
genCodeBit (Prog w h e) =
  T.concat [ "#include <stdlib.h>\n"
           , "#include <stdio.h>\n"
           , "#include <stdint.h>\n"
           , "\n"
           , "int board_is_okay(int8_t vs["
           , len
           , "]) {\n"
           , "return ("
           , codeExp e
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

codeExp :: Exp -> T.Text
codeExp e = case e of
  BitValue False -> "0\n"
  BitValue True -> "1\n"
  BoardBitValue index bit -> T.concat [ "((vs["
                                      , T.pack (show index)
                                      , "] >> "
                                      , T.pack (show bit)
                                      , ") & 1)\n"
                                      ]
  Nand e0 e1 -> T.concat [ "(("
                         , codeExp e0
                         , " & "
                         , codeExp e1
                         , ") ^ 1)\n"
                         ]
