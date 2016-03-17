module Barc.Bitter (bit) where

import qualified Barc.ExpUnrolled as E2
import qualified Barc.ExpBit as E3

import Data.Bits ((.&.))


bit :: E2.Prog -> E3.Prog
bit (E2.Prog w h e) = E3.Prog w h (fold $ bitBool e)

-- All output lists are of same length.
-- Ints are unsigned 8-bit of [2^7, 2^6, 2^5, 2^4, 2^3, 2^2, 2^1, 2^0].
bitInt :: E2.ExpInt -> [E3.Exp]
bitInt e = case e of
  E2.BoardValue i -> map (E3.BoardBitValue i) (reverse [0..7])
  E2.Const i -> map (\b -> E3.BitValue (i .&. (2^b) > 0)) (reverse [(0 :: Int)..7])
  E2.IntConv e' -> take 7 (repeat (E3.BitValue False)) ++ [bitBool e']
  E2.Add e0 e1 -> logAdd (bitInt e0) (bitInt e1)
  E2.Subtract _e0 _e1 -> error "subtract is not implemented for the bitter"
  E2.Multiply e0 e1 -> logMultiply (bitInt e0) (bitInt e1)
  E2.Modulo _e0 _e1 -> error "modulo is not implemented for the bitter"

addSub :: (E3.Exp, E3.Exp) -> ([E3.Exp], E3.Exp) -> ([E3.Exp], E3.Exp)
addSub (e0, e1) (string, carry) =
  let b = logAny [ logAll [e0, logNot e1, logNot carry]
                 , logAll [logNot e0, e1, logNot carry]
                 , logAll [logNot e0, logNot e1, carry]
                 , logAll [e0, e1, carry]
                 ]
      carry' = logAny [ logAnd e0 e1
                      , logAnd e0 carry
                      , logAnd e1 carry
                      ]
  in ([b] ++ string, carry')

bitBool :: E2.ExpBool -> E3.Exp
bitBool e = case e of
  E2.BoolVal b -> E3.BitValue b
  E2.BoolConv e' -> bitBool (E2.Gt e' (E2.Const 0))
  E2.And e0 e1 -> logAnd (bitBool e0) (bitBool e1)
  E2.Or e0 e1 -> logOr (bitBool e0) (bitBool e1)
  E2.Not e' -> logNot (bitBool e')
  E2.Eq e0 e1 -> logAll $ zipWith logEq (bitInt e0) (bitInt e1)
  E2.Gt e0 e1 -> gtSub $ zip (bitInt e0) (bitInt e1)

gtSub :: [(E3.Exp, E3.Exp)] -> E3.Exp
gtSub pairs = case pairs of
  [] -> E3.BitValue False
  [(e0, e1)] -> logAnd e0 (logNot e1)
  ((e0, e1) : es) -> logOr
                     (logAnd e0 (logNot e1))
                     (logAnd (logEq e0 e1) (gtSub es))

logNand :: E3.Exp -> E3.Exp -> E3.Exp
logNand = E3.Nand

logAnd :: E3.Exp -> E3.Exp -> E3.Exp
logAnd e0 e1 = logNot (logNand e0 e1)

logOr :: E3.Exp -> E3.Exp -> E3.Exp
logOr e0 e1 = logNot (logAnd (logNot e0) (logNot e1))

logNot :: E3.Exp -> E3.Exp
logNot e = logNand e (E3.BitValue True)

logEq :: E3.Exp -> E3.Exp -> E3.Exp
logEq e0 e1 = logOr (logAnd e0 e1) (logAnd (logNot e0) (logNot e1))

logAll :: [E3.Exp] -> E3.Exp
logAll = reduce logAnd (E3.BitValue True)

logAny :: [E3.Exp] -> E3.Exp
logAny = reduce logOr (E3.BitValue False)

logAdd :: [E3.Exp] -> [E3.Exp] -> [E3.Exp]
logAdd e0 e1 = fst $ foldr addSub ([], E3.BitValue False) $ zip e0 e1

logSum :: [[E3.Exp]] -> [E3.Exp]
logSum = reduce logAdd (take 8 (repeat (E3.BitValue False)))

logMultiply :: [E3.Exp] -> [E3.Exp] -> [E3.Exp]
logMultiply e0 e1 =
  logSum
  $ zipWith (\i b -> let n = drop i e0 ++
                             take i (repeat (E3.BitValue False))
                     in map (logAnd b) n)
  [0..] (reverse e1)

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce f ne es = case es of
  [] -> ne
  [e] -> e
  [e0, e1] -> e0 `f` e1
  _ -> let (es0, es1) = splitEqual es
       in (reduce f ne es0) `f` (reduce f ne es1)

splitEqual :: [a] -> ([a], [a])
splitEqual xs = splitAt (length xs `div` 2) xs

fold :: E3.Exp -> E3.Exp
fold e@(E3.Nand e0 e1) = case (fold e0, fold e1) of
  (E3.BitValue b0, E3.BitValue b1) -> E3.BitValue (b0 `boolNand` b1)
  (E3.BitValue False, _) -> E3.BitValue True
  (_, E3.BitValue False) -> E3.BitValue True
  _ -> e
fold e = e

boolNand :: Bool -> Bool -> Bool
boolNand x y = not (x && y)
