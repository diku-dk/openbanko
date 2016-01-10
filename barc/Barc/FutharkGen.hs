module Barc.FutharkGen (genFuthark) where

import Control.Applicative hiding (Const)
import Control.Monad.Reader
import Data.List

import Prelude

import Barc.ExpOuter

data Env = Env { envWidth :: Int
               , envHeight :: Int
               }

type GenM = ReaderT Env (Either String)

genFuthark :: Prog -> Either String String
genFuthark (Prog w h e) = do
  e' <- runReaderT (toBool <$> codeExp e) $ Env w h
  return $
    unlines [ "fun bool board_is_okay(" ++ board_t ++ " board) ="
            , futLet "xs" (futMap ("%"++show w) $ futIota $ show $ w*h) $
              futLet "ys" (futMap ("//"++show w) $ futIota $ show $ w*h) $
              futLet "vs" (futReshape [show $ w*h] "board")
              e'
            , ""
            , "fun [bool] main([" ++ board_t ++ ",n] boards) ="
            , "  map(board_is_okay, boards)"
            , ""
            , "fun bool toBool(int x) = !(x == 0)"
            , "fun int fromBool(bool x) = if x then 1 else 0"
            ]
    where w' = show w
          h' = show h
          board_t = "[[int," ++ w' ++ "], " ++ h' ++ "]"

codeExp :: Exp -> GenM String
codeExp BoardXs = return "xs"
codeExp BoardYs = return "ys"
codeExp BoardValues = return "vs"
codeExp BoardWidth = show <$> asks envWidth
codeExp BoardHeight = show <$> asks envHeight
codeExp (Const x) = return $ show x
codeExp (BoolVal b) = return (if b then "True" else "False")
codeExp (Var v) = return v

codeExp (Let v e body) =
  futLet v <$> codeExp e <*> codeExp body

codeExp (List es) =
  brackets . commaSep <$> mapM codeExp es

codeExp (Index a i) =
  futLet "tmp_arr" <$> codeExp a <*>
    (("tmp_arr"++) . brackets <$> codeExp i)

-- Now comes the ugly one.
codeExp (Length e) =
  futLet "tmp_arr" <$> codeExp e <*> pure "size(tmp_arr, 0)"

codeExp (IntConv e) = fromBool <$> codeExp e  

codeExp (BoolConv e) = toBool <$> codeExp e  

codeExp (Seq start n) = do
  start' <- codeExp start
  futMap ("+" ++ parens start') . futIota <$> codeExp n

codeExp (All e) =
  fromBool . futReduce "&&" "True" . toBoolArray <$> codeExp e

codeExp (Any e) =
  fromBool . futReduce "||" "False" . toBoolArray <$> codeExp e

codeExp (Sum e) =
  futReduce "+" "0" <$> codeExp e

codeExp (Product e) =
  futReduce "*" "1" <$> codeExp e

codeExp (Map fun e) =
  futMap <$> codeFun fun <*> codeExp e

codeExp (Reduce fun ne a) =
  futReduce <$> codeFun fun <*> codeExp ne <*> codeExp a

codeExp (Or e1 e2) =
  fromBool <$> (futBinOp "||" <$> (toBool <$> codeExp e1) <*> (toBool <$> codeExp e2))

codeExp (And e1 e2) =
  fromBool <$> (futBinOp "&&" <$> (toBool <$> codeExp e1) <*> (toBool <$> codeExp e2))

codeExp (Not e) =
  fromBool . ("!"++) . parens . toBool <$> codeExp e

codeExp (Add e1 e2) =
  futBinOp "+" <$> codeExp e1 <*> codeExp e2

codeExp (Subtract e1 e2) =
  futBinOp "-" <$> codeExp e1 <*> codeExp e2

codeExp (Multiply e1 e2) =
  futBinOp "*" <$> codeExp e1 <*> codeExp e2

codeExp (Modulo e1 e2) =
  futBinOp "%" <$> codeExp e1 <*> codeExp e2

codeExp (Eq e1 e2) =
  fromBool <$> (futBinOp "==" <$> codeExp e1 <*> codeExp e2)

codeExp (Gt e1 e2) =
  fromBool <$> (futBinOp ">" <$> codeExp e1 <*> codeExp e2)

codeExp (Lt e1 e2) =
  fromBool <$> (futBinOp "<" <$> codeExp e1 <*> codeExp e2)

codeExp (LtEq e1 e2) =
  fromBool <$> (futBinOp "<=" <$> codeExp e1 <*> codeExp e2)

codeExp (GtEq e1 e2) =
  fromBool <$> (futBinOp ">=" <$> codeExp e1 <*> codeExp e2)

codeFun :: Fun -> GenM String
codeFun (Fun params body) = do
  body' <- codeExp body
  return $ "fn int (" ++ params' ++ ") => " ++ body'
  where params' = commaSep $ map ("int " ++) params

-- Building blocks

futReduce :: String -> String -> String -> String
futReduce op ne a = "reduce" ++ parens (commaSep [op, ne, a])

futMap :: String -> String -> String
futMap fun a = "map" ++ parens (commaSep [fun, a])

futIota :: String -> String
futIota n = "iota" ++ parens n

futReshape :: [String] -> String -> String
futReshape shape e =
  "reshape" ++ parens (commaSep [parens (commaSep shape), e])

futBinOp :: String -> String -> String -> String
futBinOp op x y = parens $ unwords [x, op, y]

futLet :: String -> String -> String -> String
futLet v e body = unlines [ unwords ["let", v , "=", e, "in"]
                          , body]

parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

commaSep :: [String] -> String
commaSep = intercalate ", "

toBool :: String -> String
toBool s = "toBool" ++ parens s

fromBool :: String -> String
fromBool s = "fromBool" ++ parens s

toBoolArray :: String -> String
toBoolArray = futMap "toBool"
