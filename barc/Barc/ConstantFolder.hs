module Barc.ConstantFolder (constantFoldInt, constantFoldBool) where

import Barc.ExpUnrolled


constantFoldInt :: ExpInt -> ExpInt
constantFoldInt e = case e of
  BoardValue _ -> e
  Const _ -> e
  IntConv e' -> case constantFoldBool e' of
    BoolVal b -> if b
                 then Const 1
                 else Const 0
    _ -> e
  Add e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> Const (n0 + n1)
    (Const 0, e') -> e'
    (e', Const 0) -> e'
    _ -> e
  Subtract e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> Const (n0 - n1)
    (e', Const 0) -> e'
    _ -> e
  Multiply e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> Const (n0 * n1)
    (Const 1, e') -> e'
    (e', Const 1) -> e'
    (Const 0, _) -> Const 0
    (_, Const 0) -> Const 0
    _ -> e
  Modulo e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> Const (n0 `mod` n1)
    _ -> e

constantFoldBool :: ExpBool -> ExpBool
constantFoldBool e = case e of
  BoolVal _ -> e
  BoolConv e' -> case constantFoldInt e' of
    Const n -> if n == 0
               then BoolVal False
               else BoolVal True
    _ -> e
  And e0 e1 -> case (constantFoldBool e0, constantFoldBool e1) of
    (BoolVal b0, BoolVal b1) -> BoolVal (b0 && b1)
    (BoolVal False, _) -> BoolVal False
    (_, BoolVal False) -> BoolVal False
    _ -> e
  Or e0 e1 -> case (constantFoldBool e0, constantFoldBool e1) of
    (BoolVal b0, BoolVal b1) -> BoolVal (b0 || b1)
    (BoolVal True, _) -> BoolVal True
    (_, BoolVal True) -> BoolVal True
    _ -> e
  Not e' -> case constantFoldBool e' of
    BoolVal b -> BoolVal (not b)
    _ -> e
  Eq e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> BoolVal (n0 == n1)
    _ -> e
  Gt e0 e1 -> case (constantFoldInt e0, constantFoldInt e1) of
    (Const n0, Const n1) -> BoolVal (n0 > n1)
    _ -> e
