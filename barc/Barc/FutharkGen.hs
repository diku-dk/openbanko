{-# LANGUAGE OverloadedStrings #-}
module Barc.FutharkGen (genFuthark) where

import Data.List
import qualified Data.Text as T
import Prelude

import Barc.ExpInner

genFuthark :: Prog -> T.Text
genFuthark (Prog w h e) = 
  T.concat [ "fun bool board_is_okay("
           , board_t
           , " board) =\n"
           , futLet "vs"
             (futCall "map" ["i32", futReshape [len] "board"])
             (codeBoolExp e)
           , ""
           , "fun [bool, n] main(["
           , board_t
           , ", n] boards) =\n"
           , "map(board_is_okay, boards)\n"
           , ""
           , "fun bool toBool(i32 x) = ! (x == 0)\n"
           , "fun i32 fromBool(bool x) = if x then 1 else 0\n"
           ]
    where w' = T.pack $ show w
          h' = T.pack $ show h
          len = T.pack $ show (w * h)
          board_t = T.concat [ "[[i8, "
                             , w'
                             , "], "
                             , h'
                             , "]"
                             ]

codeBoolExp :: ExpBool -> T.Text
codeBoolExp e = T.append (codeBoolExp' e) "\n"

codeBoolExp' :: ExpBool -> T.Text
codeBoolExp' e = case e of
  BoolVal b -> if b then "True" else "False"
  IndexBool bs i -> futIndex (codeBoolListExp bs) (codeIntExp i)
  BoolConv e' -> futCall "toBool" [codeIntExp e']
  ReduceBool ident neutral list body ->
    futReduce "bool" ident (codeBoolExp neutral)
    (codeBoolListExp list) (codeBoolExp body)
  ReduceBoolValueFirst ident -> reduceValueFirst ident
  ReduceBoolValueSecond ident -> reduceValueSecond ident
  And e0 e1 -> futBinOp "&&" (codeBoolExp e0) (codeBoolExp e1)
  Or e0 e1 -> futBinOp "||" (codeBoolExp e0) (codeBoolExp e1)
  Not e' -> futUnOp "!" (codeBoolExp e')
  Eq e0 e1 -> futBinOp "==" (codeIntExp e0) (codeIntExp e1)
  Gt e0 e1 -> futBinOp ">" (codeIntExp e0) (codeIntExp e1)

codeIntExp :: ExpInt -> T.Text
codeIntExp e = T.append (codeIntExp' e) "\n"

codeIntExp' :: ExpInt -> T.Text
codeIntExp' e = case e of
  Const n -> T.pack $ show n
  IndexInt ns i -> futIndex (codeIntListExp ns) (codeIntExp i)
  CurrentIndex ident -> currentIndex ident
  LengthInt ns -> futLength $ codeIntListExp ns
  LengthBool bs -> futLength $ codeBoolListExp bs
  IntConv e' -> futCall "fromBool" [codeBoolExp e']
  ReduceInt ident neutral list body ->
    futReduce "i32" ident (codeIntExp neutral)
    (codeIntListExp list) (codeIntExp body)
  ReduceIntValueFirst ident -> reduceValueFirst ident
  ReduceIntValueSecond ident -> reduceValueSecond ident
  Add e0 e1 -> futBinOp "+" (codeIntExp e0) (codeIntExp e1)
  Subtract e0 e1 -> futBinOp "-" (codeIntExp e0) (codeIntExp e1)
  Multiply e0 e1 -> futBinOp "*" (codeIntExp e0) (codeIntExp e1)
  Modulo e0 e1 -> futBinOp "%" (codeIntExp e0) (codeIntExp e1)

codeBoolListExp :: ExpBoolList -> T.Text
codeBoolListExp e = T.append (codeBoolListExp' e) "\n"

codeBoolListExp' :: ExpBoolList -> T.Text
codeBoolListExp' e = case e of
  ListBool bs -> brackets $ commaSep $ map codeBoolExp bs
  MapBool ident len body ->
    futMap "bool" ident (codeIntExp len) (codeBoolExp body)

codeIntListExp :: ExpIntList -> T.Text
codeIntListExp e = T.append (codeIntListExp' e) "\n"

codeIntListExp' :: ExpIntList -> T.Text
codeIntListExp' e = case e of
  BoardValues -> "vs"
  ListInt ns -> brackets $ commaSep $ map codeIntExp ns
  MapInt ident len body ->
    futMap "i32" ident (codeIntExp len) (codeIntExp body)

futReduce :: T.Text -> Int -> T.Text -> T.Text -> T.Text -> T.Text
futReduce baseType ident neutral list body =
  futCall "reduce" [fun, neutral, list]
  where fun = T.concat [ "fn "
                       , baseType
                       , " "
                       , parens $ commaSep args
                       , " => "
                       , body
                       ]
        args = [ T.concat [ baseType
                          , " "
                          , reduceValueFirst ident
                          ]
               , T.concat [ baseType
                          , " "
                          , reduceValueSecond ident
                          ]
               ]

reduceValueFirst :: Int -> T.Text
reduceValueFirst ident = T.concat [ "reduce_value_first_"
                                  , T.pack $ show ident
                                  ]

reduceValueSecond :: Int -> T.Text
reduceValueSecond ident = T.concat [ "reduce_value_second_"
                                   , T.pack $ show ident
                                   ]

futMap :: T.Text -> Int -> T.Text -> T.Text -> T.Text
futMap baseType ident len body =
  futCall "map" [fun, list]
  where fun = T.concat [ "fn "
                       , baseType
                       , " "
                       , parens arg
                       , " => "
                       , body
                       ]
        arg = T.concat [ "i32 "
                       , currentIndex ident
                       ]
        list = futCall "iota" [len]

currentIndex :: Int -> T.Text
currentIndex ident = T.concat [ "current_index_"
                              , T.pack $ show ident
                              ]

futIndex :: T.Text -> T.Text -> T.Text
futIndex xs i = T.concat [ parens xs
                         , brackets i
                         ]

futCall :: T.Text -> [T.Text] -> T.Text
futCall name args = T.concat [ name
                             , parens $ commaSep args
                             ]

futLength :: T.Text -> T.Text
futLength xs = futCall "size" ["0", xs]

futBinOp :: T.Text -> T.Text -> T.Text -> T.Text
futBinOp op x y = parens $ T.concat [x, op, y]

futUnOp :: T.Text -> T.Text -> T.Text
futUnOp op x = parens $ T.concat [op, x]

futLet :: T.Text -> T.Text -> T.Text -> T.Text
futLet v e body = T.concat [ "let "
                           , v
                           , " = "
                           , e
                           , " in\n"
                           , body
                           ]

futReshape :: [T.Text] -> T.Text -> T.Text
futReshape shape e = futCall "reshape" [parens (commaSep shape), e]

parens :: T.Text -> T.Text
parens t = T.concat [ "("
                    , t
                    , ")"
                    ]

brackets :: T.Text -> T.Text
brackets t = T.concat [ "["
                      , t
                      , "]"
                      ]

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate ", "
