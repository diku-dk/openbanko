module Barc.Unroller (unroll) where

import qualified Barc.ExpInner as E1
import qualified Barc.ExpUnrolled as E2

import Control.Applicative
import qualified Data.Map as M
import Safe (atMay)
import Control.Monad

import Prelude

data Context = Context { ctxCurrentIndexes :: M.Map Int E2.Exp
                       , ctxPrevReduceValues :: M.Map Int E2.Exp
                       }
             deriving (Show)

data Static = Static { staticWidth :: Int
                     , staticHeight :: Int
                     }
            deriving (Show)

newtype UnrollM a = UnrollM { evalUnrollM :: (Static, Context) -> Either String (Context, a) }

instance Monad UnrollM where
  m >>= g = UnrollM $ \(sta, ctx) -> do
    (ctx', a) <- evalUnrollM m (sta, ctx)
    evalUnrollM (g a) (sta, ctx')
  return = pure
  fail = UnrollM . const . Left

instance Applicative UnrollM where
  pure x = UnrollM $ \(_, ctx) -> Right (ctx, x)
  (<*>) = ap

instance Functor UnrollM where
  fmap = liftM


emptyContext :: Context
emptyContext = Context M.empty M.empty

maybeFail :: String -> Maybe a -> UnrollM a
maybeFail _ (Just a) = return a
maybeFail err Nothing = fail err

getContext :: UnrollM Context
getContext = UnrollM $ \(_, ctx) -> Right (ctx, ctx)

setContext :: Context -> UnrollM ()
setContext ctx = UnrollM $ const $ Right (ctx, ())

modifyContext :: (Context -> Context) -> UnrollM ()
modifyContext f = do
  ctx <- getContext
  setContext $ f ctx

ask :: UnrollM Static
ask = UnrollM $ \(sta, ctx) -> Right (ctx, sta)

localIteration :: Int -> E2.Exp -> UnrollM a -> UnrollM a
localIteration index ite m = do
  cur <- ctxCurrentIndexes <$> getContext
  modifyContext $ \ctx -> ctx { ctxCurrentIndexes = M.insert index ite cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxCurrentIndexes = cur }
  return a

getIteration :: Int -> UnrollM E2.Exp
getIteration index = do
  iters <- ctxCurrentIndexes <$> getContext
  maybeFail "reduce index not found" $ M.lookup index iters

getPrevReduceValue :: Int -> UnrollM E2.Exp
getPrevReduceValue index = do
  vals <- ctxPrevReduceValues <$> getContext
  maybeFail "reduce index not found" $ M.lookup index vals

withPrevReduceValue :: Int -> E2.Exp -> UnrollM a -> UnrollM a
withPrevReduceValue index val m = do
  cur <- ctxPrevReduceValues <$> getContext
  modifyContext $ \ctx -> ctx { ctxPrevReduceValues = M.insert index val cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxPrevReduceValues = cur }
  return a

getBoardWidth :: UnrollM Int
getBoardWidth = staticWidth <$> ask

getBoardHeight :: UnrollM Int
getBoardHeight = staticHeight <$> ask

unroll :: E1.Prog -> Either String E2.Prog
unroll (E1.Prog w h e) = E2.Prog w h <$> unrollExp w h e

unrollExp :: Int -> Int -> E1.Exp -> Either String E2.Exp
unrollExp w h e = case e of
  E1.ListExp _ -> Left "list expression cannot be main expression"
  E1.IntExp e' -> snd <$> evalUnrollM (unrollExpM e') (Static w h, emptyContext)

unrollExpM :: E1.ExpInt -> UnrollM E2.Exp
unrollExpM e = case e of
  E1.BoardWidth -> E2.Const <$> getBoardWidth
  E1.BoardHeight -> E2.Const <$> getBoardHeight
  E1.Index eList eIndex -> do
    index <- ensureConstM "index index must be evaluable at compile time"
             =<< unrollExpM eIndex
    indexOf eList index
  E1.Const n -> return $ E2.Const n
  E1.CurrentIndex index -> getIteration index
  E1.Length eList -> lengthOf eList
  E1.Reduce v eLen eNeutral eBody -> do
    len <- ensureConstM
           "length expression in inner reduce must be evaluable at compile time"
           =<< unrollExpM eLen
    eNeutral' <- unrollExpM eNeutral
    let body :: E2.Exp -> Int -> UnrollM E2.Exp
        body p i = withPrevReduceValue v p
                   $ localIteration v (E2.Const i)
                   $ unrollExpM eBody
    foldM body eNeutral' [0..len - 1]
  E1.PrevReduceValue v -> getPrevReduceValue v
  E1.Nand e0 e1 -> do
    e0' <- unrollExpM e0
    e1' <- unrollExpM e1
    return $ case (e0', e1') of
      (E2.Const n0, E2.Const n1) -> E2.Const (if n0 > 0 && n1 > 0 then 0 else 1)
      (E2.Const n0, _) -> if n0 == 0 then E2.Const 1 else E2.Nand e0' e1'
      (_, E2.Const n1) -> if n1 == 0 then E2.Const 1 else E2.Nand e0' e1'
      _ -> E2.Nand e0' e1'
  E1.Add e0 e1 -> unrollArith E2.Add (+) e0 e1
  E1.Subtract e0 e1 -> unrollArith E2.Subtract (-) e0 e1
  E1.Multiply e0 e1 -> unrollArith E2.Multiply (*) e0 e1
  E1.Modulo e0 e1 -> unrollArith E2.Modulo mod e0 e1
  E1.Gt e0 e1 -> unrollArith E2.Gt (\a b -> boolToInt (a > b)) e0 e1
  E1.Eq e0 e1 -> unrollArith E2.Eq (\a b -> boolToInt (a == b)) e0 e1

unrollArith :: (E2.Exp -> E2.Exp -> E2.Exp)
               -> (Int -> Int -> Int)
               -> E1.ExpInt -> E1.ExpInt -> UnrollM E2.Exp
unrollArith opExp opInt e0 e1 = do
  e0' <- unrollExpM e0
  e1' <- unrollExpM e1
  return $ case (e0', e1') of
    (E2.Const n0, E2.Const n1) -> E2.Const (n0 `opInt` n1)
    _ -> e0' `opExp` e1'

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

ensureConstM :: String -> E2.Exp -> UnrollM Int
ensureConstM err e = case e of
  E2.Const n -> return n
  _ -> fail err

indexOf :: E1.ExpList -> Int -> UnrollM E2.Exp
indexOf eList index = case eList of
  E1.BoardXs -> do
    w <- getBoardWidth
    return $ E2.Const (index `mod` w)
  E1.BoardYs -> do
    w <- getBoardWidth
    return $ E2.Const (index `div` w)
  E1.BoardValues -> return $ E2.BoardValue index
  E1.List es -> case atMay es index of
    Just e -> unrollExpM e
    Nothing -> fail "list index out of range"
  E1.Map v eLen eBody -> do
    len <- ensureConstM
           "length expression in inner map must be evaluable at compile time"
           =<< unrollExpM eLen
    let body i = localIteration v i $ unrollExpM eBody
    eNew <- mapM (body . E2.Const) [0..len - 1]
    case atMay eNew index of
      Just e -> return e
      Nothing -> fail "map list index out of range"

lengthOf :: E1.ExpList -> UnrollM E2.Exp
lengthOf eList = do
  w <- getBoardWidth
  h <- getBoardHeight
  case eList of
    E1.BoardXs -> return $ E2.Const (w * h)
    E1.BoardYs -> return $ E2.Const (w * h)
    E1.BoardValues -> return $ E2.Const (w * h)
    E1.List es -> return $ E2.Const $ length es
    E1.Map _ eLen _ -> unrollExpM eLen
