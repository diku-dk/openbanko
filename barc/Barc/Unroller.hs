module Barc.Unroller (unroll) where

import qualified Barc.ExpInner as E1
import qualified Barc.ExpUnrolled as E2
import Barc.ConstantFolder (constantFoldInt, constantFoldBool)

import Control.Applicative
import qualified Data.Map as M
import Safe (atMay)
import Control.Monad

import Prelude


data Context = Context { ctxCurrentIndexes :: M.Map Int E2.ExpInt
                       , ctxReduceIntValueFirsts :: M.Map Int E2.ExpInt
                       , ctxReduceIntValueSeconds :: M.Map Int E2.ExpInt
                       , ctxReduceBoolValueFirsts :: M.Map Int E2.ExpBool
                       , ctxReduceBoolValueSeconds :: M.Map Int E2.ExpBool
                       }
             deriving (Show)

data Static = Static { staticWidth :: Int
                     , staticHeight :: Int
                     }
            deriving (Show)

newtype UnrollM a = UnrollM { evalUnrollM :: (Static, Context)
                                             -> Either String (Context, a) }

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
emptyContext = Context M.empty M.empty M.empty M.empty M.empty

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

localIteration :: Int -> E2.ExpInt -> UnrollM a -> UnrollM a
localIteration index ite m = do
  cur <- ctxCurrentIndexes <$> getContext
  modifyContext $ \ctx -> ctx { ctxCurrentIndexes = M.insert index ite cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxCurrentIndexes = cur }
  return a

getCurrentIndex :: Int -> UnrollM E2.ExpInt
getCurrentIndex index = do
  iters <- ctxCurrentIndexes <$> getContext
  maybeFail "reduce index not found" $ M.lookup index iters

getReduceIntValueFirst :: Int -> UnrollM E2.ExpInt
getReduceIntValueFirst index = do
  vals <- ctxReduceIntValueFirsts <$> getContext
  maybeFail "first reduce value index not found" $ M.lookup index vals

getReduceIntValueSecond :: Int -> UnrollM E2.ExpInt
getReduceIntValueSecond index = do
  vals <- ctxReduceIntValueSeconds <$> getContext
  maybeFail "second reduce value index not found" $ M.lookup index vals

withReduceIntValueFirst :: Int -> E2.ExpInt -> UnrollM a -> UnrollM a
withReduceIntValueFirst index val m = do
  cur <- ctxReduceIntValueFirsts <$> getContext
  modifyContext $ \ctx -> ctx { ctxReduceIntValueFirsts = M.insert index val cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxReduceIntValueFirsts = cur }
  return a

withReduceIntValueSecond :: Int -> E2.ExpInt -> UnrollM a -> UnrollM a
withReduceIntValueSecond index val m = do
  cur <- ctxReduceIntValueSeconds <$> getContext
  modifyContext $ \ctx -> ctx { ctxReduceIntValueSeconds = M.insert index val cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxReduceIntValueSeconds = cur }
  return a

getReduceBoolValueFirst :: Int -> UnrollM E2.ExpBool
getReduceBoolValueFirst index = do
  vals <- ctxReduceBoolValueFirsts <$> getContext
  maybeFail "first reduce value index not found" $ M.lookup index vals

getReduceBoolValueSecond :: Int -> UnrollM E2.ExpBool
getReduceBoolValueSecond index = do
  vals <- ctxReduceBoolValueSeconds <$> getContext
  maybeFail "second reduce value index not found" $ M.lookup index vals

withReduceBoolValueFirst :: Int -> E2.ExpBool -> UnrollM a -> UnrollM a
withReduceBoolValueFirst index val m = do
  cur <- ctxReduceBoolValueFirsts <$> getContext
  modifyContext $ \ctx -> ctx { ctxReduceBoolValueFirsts = M.insert index val cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxReduceBoolValueFirsts = cur }
  return a

withReduceBoolValueSecond :: Int -> E2.ExpBool -> UnrollM a -> UnrollM a
withReduceBoolValueSecond index val m = do
  cur <- ctxReduceBoolValueSeconds <$> getContext
  modifyContext $ \ctx -> ctx { ctxReduceBoolValueSeconds = M.insert index val cur }
  a <- m
  modifyContext $ \ctx -> ctx { ctxReduceBoolValueSeconds = cur }
  return a

getBoardWidth :: UnrollM Int
getBoardWidth = staticWidth <$> ask

getBoardHeight :: UnrollM Int
getBoardHeight = staticHeight <$> ask

unroll :: E1.Prog -> Either String E2.Prog
unroll (E1.Prog w h e) = E2.Prog w h <$> unrollExp w h e

unrollExp :: Int -> Int -> E1.ExpBool -> Either String E2.ExpBool
unrollExp w h e = snd <$> evalUnrollM (unrollBoolM e) (Static w h, emptyContext)

unrollIntM :: E1.ExpInt -> UnrollM E2.ExpInt
unrollIntM e = constantFoldInt <$> unrollIntM' e

unrollIntM' :: E1.ExpInt -> UnrollM E2.ExpInt
unrollIntM' e = case e of
  E1.Const n -> return $ E2.Const n
  E1.IndexInt eList eIndex -> do
    index <- ensureConstM =<< unrollIntM eIndex
    indexIntOf eList index
  E1.CurrentIndex v -> getCurrentIndex v
  E1.LengthInt es -> lengthIntOf es
  E1.LengthBool es -> lengthBoolOf es
  E1.IntConv e' -> E2.IntConv <$> unrollBoolM e'
  E1.ReduceInt v eList eNeutral eBody -> reduceIntHandle v eList eNeutral eBody
  E1.ReduceIntValueFirst v -> getReduceIntValueFirst v
  E1.ReduceIntValueSecond v -> getReduceIntValueSecond v
  E1.Add e0 e1 -> E2.Add <$> unrollIntM e0 <*> unrollIntM e1
  E1.Subtract e0 e1 -> E2.Subtract <$> unrollIntM e0 <*> unrollIntM e1
  E1.Multiply e0 e1 -> E2.Multiply <$> unrollIntM e0 <*> unrollIntM e1
  E1.Modulo e0 e1 -> E2.Modulo <$> unrollIntM e0 <*> unrollIntM e1

unrollBoolM :: E1.ExpBool -> UnrollM E2.ExpBool
unrollBoolM e = constantFoldBool <$> unrollBoolM' e

unrollBoolM' :: E1.ExpBool -> UnrollM E2.ExpBool
unrollBoolM' e = case e of
  E1.BoolVal b -> return $ E2.BoolVal b
  E1.IndexBool eList eIndex -> do
    index <- ensureConstM =<< unrollIntM eIndex
    indexBoolOf eList index
  E1.BoolConv e' -> E2.BoolConv <$> unrollIntM e'
  E1.ReduceBool v eList eNeutral eBody -> reduceBoolHandle v eList eNeutral eBody
  E1.ReduceBoolValueFirst v -> getReduceBoolValueFirst v
  E1.ReduceBoolValueSecond v -> getReduceBoolValueSecond v
  E1.And e0 e1 -> E2.And <$> unrollBoolM e0 <*> unrollBoolM e1
  E1.Or e0 e1 -> E2.Or <$> unrollBoolM e0 <*> unrollBoolM e1
  E1.Not e' -> E2.Not <$> unrollBoolM e'
  E1.Eq e0 e1 -> E2.Eq <$> unrollIntM e0 <*> unrollIntM e1
  E1.Gt e0 e1 -> E2.Gt <$> unrollIntM e0 <*> unrollIntM e1

ensureConstM :: E2.ExpInt -> UnrollM Int
ensureConstM e = case e of
  E2.Const n -> return n
  _ -> fail ("wanted constant, but got " ++ show e)

reduceIntHandle :: Int -> E1.ExpInt -> E1.ExpIntList -> E1.ExpInt
                   -> UnrollM E2.ExpInt
reduceIntHandle v eNeutral eList eBody = do
  eNeutral' <- unrollIntM eNeutral
  let body :: E2.ExpInt -> E2.ExpInt -> UnrollM E2.ExpInt
      body rvalFirst rvalSecond = withReduceIntValueFirst v rvalFirst
                                  $ withReduceIntValueSecond v rvalSecond
                                  $ unrollIntM eBody
  eListLen <- ensureConstM =<< unrollIntM (E1.LengthInt eList)
  let is = [0..eListLen - 1]
  es <- mapM (indexIntOf eList) is
  halve body eNeutral' es

reduceBoolHandle :: Int -> E1.ExpBool -> E1.ExpBoolList -> E1.ExpBool
                    -> UnrollM E2.ExpBool
reduceBoolHandle v eNeutral eList eBody = do
  eNeutral' <- unrollBoolM eNeutral
  let body :: E2.ExpBool -> E2.ExpBool -> UnrollM E2.ExpBool
      body rvalFirst rvalSecond = withReduceBoolValueFirst v rvalFirst
                                  $ withReduceBoolValueSecond v rvalSecond
                                  $ unrollBoolM eBody
  eListLen <- ensureConstM =<< unrollIntM (E1.LengthBool eList)
  let is = [0..eListLen - 1]
  es <- mapM (indexBoolOf eList) is
  halve body eNeutral' es

halve :: Monad m => (a -> a -> m a) -> a -> [a] -> m a
halve f ne xs = case xs of
  [] -> return ne
  [x] -> return x
  [x0, x1] -> f x0 x1
  _ -> do
    let (a, b) = splitEqual xs
    xs0 <- halve f ne a
    xs1 <- halve f ne b
    f xs0 xs1

splitEqual :: [a] -> ([a], [a])
splitEqual xs = splitAt (length xs `div` 2) xs

indexIntOf :: E1.ExpIntList -> Int -> UnrollM E2.ExpInt
indexIntOf eList index = case eList of
  E1.BoardValues -> return $ E2.BoardValue index
  E1.ListInt es -> case atMay es index of
    Just e -> unrollIntM e
    Nothing -> fail "list index out of range"
  E1.MapInt v eLen eBody -> do
    len <- ensureConstM =<< unrollIntM eLen
    let body i = localIteration v i $ unrollIntM eBody
    eNew <- mapM (body . E2.Const) [0..len - 1]
    case atMay eNew index of
      Just e -> return e
      Nothing -> fail "map list index out of range"

indexBoolOf :: E1.ExpBoolList -> Int -> UnrollM E2.ExpBool
indexBoolOf eList index = case eList of
  E1.ListBool es -> case atMay es index of
    Just e -> unrollBoolM e
    Nothing -> fail "list index out of range"
  E1.MapBool v eLen eBody -> do
    len <- ensureConstM =<< unrollIntM eLen
    let body i = localIteration v i $ unrollBoolM eBody
    eNew <- mapM (body . E2.Const) [0..len - 1]
    case atMay eNew index of
      Just e -> return e
      Nothing -> fail "map list index out of range"

lengthIntOf :: E1.ExpIntList -> UnrollM E2.ExpInt
lengthIntOf eList = do
  case eList of
    E1.BoardValues -> do
      w <- getBoardWidth
      h <- getBoardHeight
      return $ E2.Const (w * h)
    E1.ListInt es -> return $ E2.Const $ length es
    E1.MapInt _ eLen _ -> unrollIntM eLen

lengthBoolOf :: E1.ExpBoolList -> UnrollM E2.ExpInt
lengthBoolOf eList = do
  case eList of
    E1.ListBool es -> return $ E2.Const $ length es
    E1.MapBool _ eLen _ -> unrollIntM eLen
