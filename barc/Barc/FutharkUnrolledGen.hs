{-# LANGUAGE OverloadedStrings #-}
module Barc.FutharkUnrolledGen (genFutharkUnrolled) where

import qualified Data.Text as T

import Barc.ExpUnrolled


-- Using i8 for everything is actually cheating, since we don't know that this
-- will work in the general case.  But for banko, it's good enough.

genFutharkUnrolled :: Prog -> T.Text
genFutharkUnrolled (Prog w h e) = 
  T.concat [ "fun bool board_is_okay("
           , "[i8, "
           , len
           , "] board) =\n"
           , codeBoolExp e
           , ""
           , "fun [bool, n] main([[[i8, "
           , w'
           , "], "
           , h'
           , "], n] boards) =\n"
           , "map(board_is_okay, reshape((n, "
           , len
           , "), boards))\n"
           , ""
           , "fun bool toBool(i8 x) = ! (x == 0i8)\n"
           , "fun i8 fromBool(bool x) = if x then 1i8 else 0i8\n"
           ]
    where w' = T.pack $ show w
          h' = T.pack $ show h
          len = T.pack $ show (w * h)

codeBoolExp :: ExpBool -> T.Text
codeBoolExp e = T.append (codeBoolExp' e) "\n"

codeBoolExp' :: ExpBool -> T.Text
codeBoolExp' e = case e of
  BoolVal b -> if b then "True" else "False"
  BoolConv i -> T.concat [ "toBool("
                         , codeIntExp i
                         , ")"
                         ]
  And e0 e1 -> futBinOp "&&" (codeBoolExp e0) (codeBoolExp e1)
  Or e0 e1 -> futBinOp "||" (codeBoolExp e0) (codeBoolExp e1)
  Not e' -> T.concat [ "!"
                     , parens $ codeBoolExp e'
                     ]
  Eq e0 e1 -> futBinOp "==" (codeIntExp e0) (codeIntExp e1)
  Gt e0 e1 -> futBinOp ">" (codeIntExp e0) (codeIntExp e1)

codeIntExp :: ExpInt -> T.Text
codeIntExp e = T.append (codeIntExp' e) "\n"

codeIntExp' :: ExpInt -> T.Text
codeIntExp' e = case e of
  BoardValue i -> T.concat [ "board["
                           , T.pack $ show i
                           , "]"
                           ]
  Const n -> T.concat [ T.pack $ show n
                      , "i8"
                      ]
  IntConv b -> T.concat [ "fromBool("
                        , codeBoolExp b
                        , ")"
                        ]
  Add e0 e1 -> futBinOp "+" (codeIntExp e0) (codeIntExp e1)
  Subtract e0 e1 -> futBinOp "-" (codeIntExp e0) (codeIntExp e1)
  Multiply e0 e1 -> futBinOp "*" (codeIntExp e0) (codeIntExp e1)
  Modulo e0 e1 -> futBinOp "%" (codeIntExp e0) (codeIntExp e1)

futBinOp :: T.Text -> T.Text -> T.Text -> T.Text
futBinOp op x y = parens $ T.concat [x, op, y]

parens :: T.Text -> T.Text
parens t = T.concat [ "("
                    , t
                    , ")"
                    ]
