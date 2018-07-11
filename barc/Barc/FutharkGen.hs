{-# LANGUAGE OverloadedStrings #-}
module Barc.FutharkGen (genFuthark) where

import Data.List
import qualified Data.Text as T
import Prelude

import Barc.ExpInner

genFuthark :: Prog -> T.Text
genFuthark (Prog w h e) =
  T.concat [ "let board_is_okay(board: "
           , board_t
           , "): bool =\n"
           , futLet "vs"
             (futCall "map" ["i32.i8", futCall "flatten" ["board"]])
             (codeBoolExp e)
           , ""
           , "let main [n] (boards: [n]"
           , board_t
           , "): [n]bool =\n"
           , "  map board_is_okay boards\n"
           ]
    where w' = T.pack $ show w
          h' = T.pack $ show h
          len = T.pack $ show (w * h)
          board_t = T.concat [ "[", h', "][", w', "]i8"]

codeBoolExp :: ExpBool -> T.Text
codeBoolExp e = T.append (codeBoolExp' e) "\n"

codeBoolExp' :: ExpBool -> T.Text
codeBoolExp' e = case e of
  BoolVal b -> if b then "true" else "false"
  IndexBool bs i -> futIndex (codeBoolListExp bs) (codeIntExp i)
  BoolConv e' -> futCall "i32.bool" [codeIntExp e']
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
  IntConv e' -> futCall "i32.bool" [codeBoolExp e']
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
  where fun = T.concat [ "\\"
                       , T.concat $ map parens args
                       , ": "
                       , baseType
                       , " -> "
                       , body
                       ]
        args = [ T.concat [ reduceValueFirst ident
                          , ": "
                          , baseType
                          ]
               , T.concat [ reduceValueSecond ident
                          , ": "
                          , baseType
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
  where fun = T.concat [ "\\"
                       , parens arg
                       , ": "
                       , baseType
                       , " -> "
                       , body
                       ]
        arg = T.concat [ currentIndex ident
                       , ": i32"
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
futCall name args = name <> mconcat (map parens args)

futLength :: T.Text -> T.Text
futLength xs = futCall "length" [xs]

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
