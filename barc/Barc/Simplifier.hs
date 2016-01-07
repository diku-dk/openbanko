module Barc.Simplifier (simplify) where

import qualified Barc.ExpOuter as E0
import qualified Barc.ExpInner as E1

import Control.Applicative
import qualified Data.Map as M
import Control.Monad

import Prelude

type Bindings = M.Map String E1.Exp

data Context = Context { ctxBindings :: Bindings
                       , ctxNewNameIdCurrent :: Int
                       , ctxIndexIdCurrent :: Int
                       }
             deriving (Show)

newtype SimplifyM a = SimplifyM { evalSimplifyM :: Context -> Either String (Context, a) }

instance Monad SimplifyM where
  m >>= g = SimplifyM $ \ctx -> do
    (ctx', a) <- evalSimplifyM m ctx
    evalSimplifyM (g a) ctx'
  return = pure
  fail = SimplifyM . const . Left

instance Applicative SimplifyM where
  pure x = SimplifyM $ \bs -> Right (bs, x)
  (<*>) = ap

instance Functor SimplifyM where
  fmap = liftM

emptyContext :: Context
emptyContext = Context M.empty 0 0

maybeFail :: String -> Maybe a -> SimplifyM a
maybeFail _ (Just a) = return a
maybeFail err Nothing = fail err

getContext :: SimplifyM Context
getContext = SimplifyM $ \ctx -> Right (ctx, ctx)

setContext :: Context -> SimplifyM ()
setContext ctx = SimplifyM $ const $ Right (ctx, ())

modifyContext :: (Context -> Context) -> SimplifyM ()
modifyContext f = do
  ctx <- getContext
  setContext $ f ctx

lookupM :: String -> SimplifyM E1.Exp
lookupM v = do
  ctx <- getContext
  maybeFail ("binding not found: " ++ v) $ M.lookup v (ctxBindings ctx)

newName :: String -> SimplifyM String
newName base = do
  ctx <- getContext
  let i = ctxNewNameIdCurrent ctx
  setContext ctx { ctxNewNameIdCurrent = i + 1 }
  return (base ++ "_" ++ show i)

newIndexId :: SimplifyM Int
newIndexId = do
  ctx <- getContext
  let i = ctxIndexIdCurrent ctx
  setContext ctx { ctxIndexIdCurrent = i + 1 }
  return i

bind :: String -> E1.Exp -> SimplifyM ()
bind v e = do
  ctx <- getContext
  setContext ctx { ctxBindings = M.insert v e $ ctxBindings ctx }

localBindings :: SimplifyM a -> SimplifyM a
localBindings m = do
  bindings0 <- ctxBindings <$> getContext
  a <- m
  modifyContext $ \ctx -> ctx { ctxBindings = bindings0 }
  return a

simplify :: E0.Prog -> Either String E1.Prog
simplify (E0.Prog w h e) = E1.Prog w h <$> simplifyExp e

simplifyExp :: E0.Exp -> Either String E1.Exp
simplifyExp e = snd <$> evalSimplifyM (simplifyM e) emptyContext

ensureIntM :: String -> E1.Exp -> SimplifyM E1.ExpInt
ensureIntM err e = case e of
  E1.IntExp ei -> return ei
  _ -> fail err

ensureListM :: String -> E1.Exp -> SimplifyM E1.ExpList
ensureListM err e = case e of
  E1.ListExp el -> return el
  _ -> fail err

simplifyM :: E0.Exp -> SimplifyM E1.Exp
simplifyM e = case e of
  E0.BoardXs -> return $ E1.ListExp E1.BoardXs
  E0.BoardYs -> return $ E1.ListExp E1.BoardYs
  E0.BoardValues -> return $ E1.ListExp E1.BoardValues
  E0.BoardWidth -> return $ E1.IntExp E1.BoardWidth
  E0.BoardHeight -> return $ E1.IntExp E1.BoardHeight
  E0.Let v eVal eBody -> do
    eVal' <- simplifyM eVal
    localBindings $ do
      bind v eVal'
      simplifyM eBody
  E0.Const n -> return $ E1.IntExp $ E1.Const n
  E0.List es -> E1.ListExp <$>
                (E1.List <$>
                 (mapM (ensureIntM "list expression element must be int") =<<
                  mapM simplifyM es))
  E0.Var v -> lookupM v
  E0.Index e0 i -> E1.IntExp <$>
                  (E1.Index
                   <$> (ensureListM "index expression must be list" =<< simplifyM e0)
                   <*> (ensureIntM "index index must be int" =<< simplifyM i))
  E0.Length e0 -> E1.IntExp <$> (E1.Length
                                <$> (ensureListM "length expression must be list" =<< simplifyM e0))
  E0.Seq e0 e1 -> do
    diff <- E1.Subtract
            <$> (ensureIntM "seq arguments must be ints" =<< simplifyM e1)
            <*> (ensureIntM "seq arguments must be ints" =<< simplifyM e0)
    indexId <- newIndexId
    return $ E1.ListExp $ E1.Map indexId diff $ E1.CurrentIndex indexId
  E0.All e0 -> do
    a <- newName "all_a"
    b <- newName "all_b"
    simplifyM (E0.Reduce (E0.Fun [a, b]
                      $ E0.And (E0.Var a) (E0.Var b))
           (E0.Const 1) e0)
  E0.Any e0 -> do
    a <- newName "any_a"
    simplifyM $ E0.Not $ E0.All
      $ E0.Map (E0.Fun [a] $ E0.Not (E0.Var a)) e0
  E0.Sum e0 -> do
    a <- newName "sum_a"
    b <- newName "sum_b"
    simplifyM (E0.Reduce (E0.Fun [a, b]
                      $ E0.Add (E0.Var a) (E0.Var b))
           (E0.Const 0) e0)
  E0.Product e0 -> do
    a <- newName "sum_a"
    b <- newName "sum_b"
    simplifyM (E0.Reduce (E0.Fun [a, b]
                      $ E0.Multiply (E0.Var a) (E0.Var b))
           (E0.Const 1) e0)
  E0.Map f e0 -> do
    e' <- ensureListM "map source must be list" =<< simplifyM e0
    indexId <- newIndexId
    fe <- ensureIntM "map body must be int" =<< simplifyFunMapM f indexId e'
    return $ E1.ListExp $ E1.Map indexId (E1.Length e') fe
  E0.Reduce f e0 e1 -> do
    e0' <- ensureIntM "reduce neutral element must be int" =<< simplifyM e0
    e1' <- ensureListM "reduce source must be list" =<< simplifyM e1
    indexId <- newIndexId
    fe <- ensureIntM "reduce body must be int" =<< simplifyFunReduceM f indexId e1'
    return $ E1.IntExp $ E1.Reduce indexId (E1.Length e1') e0' fe
  E0.And e0 e1 -> E1.IntExp <$>
                  (logAnd
                   <$> (ensureIntM "'and' args must be int" =<< simplifyM e0)
                   <*> (ensureIntM "'and' args must be int" =<< simplifyM e1))
  E0.Or e0 e1 -> E1.IntExp <$>
                 (logOr
                  <$> (ensureIntM "'or' args must be int" =<< simplifyM e0)
                  <*> (ensureIntM "'or' args must be int" =<< simplifyM e1))
  E0.Not e0 -> E1.IntExp <$>
              (logNot <$> (ensureIntM "'not' arg must be int" =<< simplifyM e0))
  E0.Add e0 e1 -> arith E1.Add e0 e1
  E0.Subtract e0 e1 -> arith E1.Subtract e0 e1
  E0.Multiply e0 e1 -> arith E1.Multiply e0 e1
  E0.Modulo e0 e1 -> arith E1.Modulo e0 e1
  E0.Eq e0 e1 -> arith E1.Eq e0 e1
  E0.Gt e0 e1 -> arith E1.Gt e0 e1
  E0.GtEq e0 e1 -> simplifyM $ E0.Or (E0.Gt e0 e1) (E0.Eq e0 e1)
  E0.Lt e0 e1 -> simplifyM $ E0.Gt e1 e0
  E0.LtEq e0 e1 -> simplifyM $ E0.GtEq e1 e0

arith :: (E1.ExpInt -> E1.ExpInt -> E1.ExpInt) -> E0.Exp -> E0.Exp -> SimplifyM E1.Exp
arith op a b = do
  a' <- ensureIntM "arithmetic operator argument must be int" =<< simplifyM a
  b' <- ensureIntM "arithmetic operator argument must be int" =<< simplifyM b
  return $ E1.IntExp (a' `op` b')

logAnd :: E1.ExpInt -> E1.ExpInt -> E1.ExpInt
logAnd a b = logNot $ logNand a b

logOr :: E1.ExpInt -> E1.ExpInt -> E1.ExpInt
logOr a b = logNot $ logAnd (logNot a) (logNot b)

logNot :: E1.ExpInt -> E1.ExpInt
logNot a = logNand a (E1.Const 1)

logNand :: E1.ExpInt -> E1.ExpInt -> E1.ExpInt
logNand = E1.Nand

simplifyFunMapM :: E0.Fun -> Int -> E1.ExpList -> SimplifyM E1.Exp
simplifyFunMapM (E0.Fun args e) indexId eSrc = localBindings $ do
  case args of
    [arg] ->
      bind arg $ E1.IntExp $ E1.Index eSrc (E1.CurrentIndex indexId)
    _ -> fail "map function must take one argument"
  simplifyM e

simplifyFunReduceM :: E0.Fun -> Int -> E1.ExpList -> SimplifyM E1.Exp
simplifyFunReduceM (E0.Fun args e) indexId eSrc = localBindings $ do
  case args of
    [arg0, arg1] -> do
      bind arg0 $ E1.IntExp $ E1.PrevReduceValue indexId
      bind arg1 $ E1.IntExp $ E1.Index eSrc (E1.CurrentIndex indexId)
    _ -> fail "reduce function must take two arguments"
  simplifyM e
