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

data Static = Static { staticWidth :: Int
                     , staticHeight :: Int
                     }
            deriving (Show)

newtype SimplifyM a = SimplifyM { evalSimplifyM :: (Static, Context)
                                                -> Either String (Context, a) }

instance Monad SimplifyM where
  m >>= g = SimplifyM $ \(sta, ctx) -> do
    (ctx', a) <- evalSimplifyM m (sta, ctx)
    evalSimplifyM (g a) (sta, ctx')
  return = pure
  fail = SimplifyM . const . Left

instance Applicative SimplifyM where
  pure x = SimplifyM $ \(_, bs) -> Right (bs, x)
  (<*>) = ap

instance Functor SimplifyM where
  fmap = liftM

instance Alternative SimplifyM where
  empty = fail "abuse"
  m <|> n = SimplifyM $ \(sta, ctx) -> case evalSimplifyM m (sta, ctx) of
    Right t -> Right t
    Left _ -> evalSimplifyM n (sta, ctx)

emptyContext :: Context
emptyContext = Context M.empty 0 0

maybeFail :: String -> Maybe a -> SimplifyM a
maybeFail _ (Just a) = return a
maybeFail err Nothing = fail err

getContext :: SimplifyM Context
getContext = SimplifyM $ \(_, ctx) -> Right (ctx, ctx)

setContext :: Context -> SimplifyM ()
setContext ctx = SimplifyM $ const $ Right (ctx, ())

modifyContext :: (Context -> Context) -> SimplifyM ()
modifyContext f = do
  ctx <- getContext
  setContext $ f ctx

ask :: SimplifyM Static
ask = SimplifyM $ \(sta, ctx) -> Right (ctx, sta)

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

getBoardWidth :: SimplifyM Int
getBoardWidth = staticWidth <$> ask

getBoardHeight :: SimplifyM Int
getBoardHeight = staticHeight <$> ask

localBindings :: SimplifyM a -> SimplifyM a
localBindings m = do
  bindings0 <- ctxBindings <$> getContext
  a <- m
  modifyContext $ \ctx -> ctx { ctxBindings = bindings0 }
  return a

simplify :: E0.Prog -> Either String E1.Prog
simplify (E0.Prog w h e) = E1.Prog w h <$> (simplifyExp w h e)

simplifyExp :: Int -> Int -> E0.Exp -> Either String E1.ExpBool
simplifyExp w h e = snd <$> (evalSimplifyM (simplifyBoolM e)
                             (Static w h, emptyContext))

simplifyM :: E0.Exp -> SimplifyM E1.Exp
simplifyM e = (E1.IntListExp <$> simplifyIntListM e) <|>
              (E1.BoolListExp <$> simplifyBoolListM e) <|>
              (E1.IntExp <$> simplifyIntM e) <|>
              (E1.BoolExp <$> simplifyBoolM e)

simplifyIntListM :: E0.Exp -> SimplifyM E1.ExpIntList
simplifyIntListM e = case e of
  E0.BoardXs -> do
    w <- getBoardWidth
    h <- getBoardHeight
    return $ E1.ListInt $ map E1.Const $ map (\i -> i `mod` w) [0..w * h - 1]
  E0.BoardYs -> do
    w <- getBoardWidth
    h <- getBoardHeight
    return $ E1.ListInt $ map E1.Const $ map (\i -> i `div` w) [0..w * h - 1]
  E0.BoardValues -> return E1.BoardValues
  E0.Let v eVal eBody -> letHandle v eVal eBody simplifyIntListM
  E0.Var v -> ensureIntListM =<< lookupM v
  E0.List es -> E1.ListInt <$> mapM simplifyIntM es
  E0.Seq e0 e1 -> do
    diff <- E1.Subtract <$> simplifyIntM e1 <*> simplifyIntM e0
    indexId <- newIndexId
    return $ E1.MapInt indexId diff $ E1.CurrentIndex indexId
  E0.Map fun es -> mapIntHandle fun es
  _ -> fail ("expected int list, but got " ++ show e)
  
simplifyBoolListM :: E0.Exp -> SimplifyM E1.ExpBoolList
simplifyBoolListM e = case e of
  E0.Let v eVal eBody -> letHandle v eVal eBody simplifyBoolListM
  E0.Var v -> ensureBoolListM =<< lookupM v
  E0.List es -> E1.ListBool <$> mapM simplifyBoolM es
  E0.Map fun es -> mapBoolHandle fun es
  _ -> fail ("expected bool list, but got " ++ show e)

simplifyIntM :: E0.Exp -> SimplifyM E1.ExpInt
simplifyIntM e = case e of
  E0.BoardWidth -> E1.Const <$> getBoardWidth
  E0.BoardHeight -> E1.Const <$> getBoardHeight
  E0.Let v eVal eBody -> letHandle v eVal eBody simplifyIntM
  E0.Var v -> ensureIntM =<< lookupM v
  E0.Const n -> return $ E1.Const n
  E0.Index eList eIndex -> E1.IndexInt
                           <$> simplifyIntListM eList
                           <*> simplifyIntM eIndex
  E0.Length eList -> (E1.LengthInt <$> simplifyIntListM eList)
                     <|> (E1.LengthBool <$> simplifyBoolListM eList)
  E0.IntConv e' -> E1.IntConv <$> simplifyBoolM e'
  E0.Sum es -> asReduceHandle E0.Add (E0.Const 0) es simplifyIntM
  E0.Product es -> asReduceHandle E0.Multiply (E0.Const 1) es simplifyIntM
  E0.Reduce fun ne es -> reduceIntHandle fun ne es
  E0.Add e0 e1 -> E1.Add <$> simplifyIntM e0 <*> simplifyIntM e1
  E0.Subtract e0 e1 -> E1.Subtract <$> simplifyIntM e0 <*> simplifyIntM e1
  E0.Multiply e0 e1 -> E1.Multiply <$> simplifyIntM e0 <*> simplifyIntM e1
  E0.Modulo e0 e1 -> E1.Modulo <$> simplifyIntM e0 <*> simplifyIntM e1
  _ -> fail ("expected int, but got " ++ show e)

simplifyBoolM :: E0.Exp -> SimplifyM E1.ExpBool
simplifyBoolM e = case e of
  E0.Let v eVal eBody -> letHandle v eVal eBody simplifyBoolM
  E0.Var v -> ensureBoolM =<< lookupM v
  E0.BoolVal b -> return $ E1.BoolVal b
  E0.Index eList eIndex -> E1.IndexBool
                           <$> simplifyBoolListM eList
                           <*> simplifyIntM eIndex
  E0.BoolConv e' -> E1.BoolConv <$> simplifyIntM e'
  E0.All es -> asReduceHandle E0.And (E0.BoolVal True) es simplifyBoolM
  E0.Any es -> asReduceHandle E0.Or (E0.BoolVal False) es simplifyBoolM
  E0.Reduce fun ne es -> reduceBoolHandle fun ne es
  E0.And e0 e1 -> E1.And <$> simplifyBoolM e0 <*> simplifyBoolM e1
  E0.Or e0 e1 -> E1.Or <$> simplifyBoolM e0 <*> simplifyBoolM e1
  E0.Not e' -> E1.Not <$> simplifyBoolM e'
  E0.Eq e0 e1 -> E1.Eq <$> simplifyIntM e0 <*> simplifyIntM e1
  E0.Gt e0 e1 -> E1.Gt <$> simplifyIntM e0 <*> simplifyIntM e1
  E0.GtEq e0 e1 -> simplifyBoolM $ E0.Or (E0.Gt e0 e1) (E0.Eq e0 e1)
  E0.Lt e0 e1 -> simplifyBoolM $ E0.Gt e1 e0
  E0.LtEq e0 e1 -> simplifyBoolM $ E0.GtEq e1 e0
  _ -> fail ("expected bool, but got " ++ show e)

ensureIntListM :: E1.Exp -> SimplifyM E1.ExpIntList
ensureIntListM e = case e of
  E1.IntListExp e' -> return e'
  _ -> fail ("wanted int list, but got " ++ show e)

ensureBoolListM :: E1.Exp -> SimplifyM E1.ExpBoolList
ensureBoolListM e = case e of
  E1.BoolListExp e' -> return e'
  _ -> fail ("wanted bool list, but got " ++ show e)

ensureIntM :: E1.Exp -> SimplifyM E1.ExpInt
ensureIntM e = case e of
  E1.IntExp e' -> return e'
  _ -> fail ("wanted int, but got " ++ show e)

ensureBoolM :: E1.Exp -> SimplifyM E1.ExpBool
ensureBoolM e = case e of
  E1.BoolExp e' -> return e'
  _ -> fail ("wanted bool, but got " ++ show e)

letHandle :: String -> E0.Exp -> E0.Exp -> (E0.Exp -> SimplifyM a) -> SimplifyM a
letHandle v eVal eBody simplifyFunM = do
  eVal' <- simplifyM eVal
  localBindings $ do
    bind v eVal'
    simplifyFunM eBody

asReduceHandle :: (E0.Exp -> E0.Exp -> E0.Exp)
                  -> E0.Exp -> E0.Exp -> (E0.Exp -> SimplifyM a) -> SimplifyM a
asReduceHandle binOp ne es simplifyFunM = do
  a <- newName "all_a"
  b <- newName "all_b"
  simplifyFunM (E0.Reduce (E0.Fun [a, b]
                           $ binOp (E0.Var a) (E0.Var b))
                ne es)

reduceIntHandle :: E0.Fun -> E0.Exp -> E0.Exp -> SimplifyM E1.ExpInt
reduceIntHandle (E0.Fun [arg0, arg1] eBody) ne es = do
  ne' <- simplifyIntM ne
  es' <- simplifyIntListM es
  indexId <- newIndexId
  eBody' <- localBindings $ do
    bind arg0 (E1.IntExp $ E1.ReduceIntValueFirst indexId)
    bind arg1 (E1.IntExp $ E1.ReduceIntValueSecond indexId)
    simplifyIntM eBody
  return $ E1.ReduceInt indexId ne' es' eBody'
reduceIntHandle _ _ _ = fail "reduce function must take exactly two arguments"

reduceBoolHandle :: E0.Fun -> E0.Exp -> E0.Exp -> SimplifyM E1.ExpBool
reduceBoolHandle (E0.Fun [arg0, arg1] eBody) ne es = do
  ne' <- simplifyBoolM ne
  es' <- simplifyBoolListM es
  indexId <- newIndexId
  eBody' <- localBindings $ do
    bind arg0 (E1.BoolExp $ E1.ReduceBoolValueFirst indexId)
    bind arg1 (E1.BoolExp $ E1.ReduceBoolValueSecond indexId)
    simplifyBoolM eBody
  return $ E1.ReduceBool indexId ne' es' eBody'
reduceBoolHandle _ _ _ = fail "reduce function must take exactly two arguments"

mapIntHandle :: E0.Fun -> E0.Exp -> SimplifyM E1.ExpIntList
mapIntHandle (E0.Fun [arg] eBody) es = tryIntSrc <|> tryBoolSrc
  where tryBoolSrc = do
          es' <- simplifyBoolListM es
          indexId <- newIndexId
          eBody' <- localBindings $ do
            bind arg (E1.BoolExp (E1.IndexBool es' (E1.CurrentIndex indexId)))
            simplifyIntM eBody
          return $ E1.MapInt indexId (E1.LengthBool es') eBody'

        tryIntSrc = do
          es' <- simplifyIntListM es
          indexId <- newIndexId
          eBody' <- localBindings $ do
            bind arg (E1.IntExp (E1.IndexInt es' (E1.CurrentIndex indexId)))
            simplifyIntM eBody
          return $ E1.MapInt indexId (E1.LengthInt es') eBody'
mapIntHandle _ _ = fail "map function must take exactly one argument"

mapBoolHandle :: E0.Fun -> E0.Exp -> SimplifyM E1.ExpBoolList
mapBoolHandle (E0.Fun [arg] eBody) es = tryIntSrc <|> tryBoolSrc
  where tryBoolSrc = do
          es' <- simplifyBoolListM es
          indexId <- newIndexId
          eBody' <- localBindings $ do
            bind arg (E1.BoolExp (E1.IndexBool es' (E1.CurrentIndex indexId)))
            simplifyBoolM eBody
          return $ E1.MapBool indexId (E1.LengthBool es') eBody'

        tryIntSrc = do
          es' <- simplifyIntListM es
          indexId <- newIndexId
          eBody' <- localBindings $ do
            bind arg (E1.IntExp (E1.IndexInt es' (E1.CurrentIndex indexId)))
            simplifyBoolM eBody
          return $ E1.MapBool indexId (E1.LengthInt es') eBody'
mapBoolHandle _ _ = fail "map function must take exactly one argument"
