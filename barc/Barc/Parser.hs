{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Barc.Parser (parseString) where

import Barc.ExpOuter

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad
import Control.Applicative hiding (many, (<|>), Const)

import Prelude

parseString :: String -> Either ParseError Prog
parseString = parse progP ""

spaceAndComments :: Parser ()
spaceAndComments = void $ many (many1 space <|> between (char '#') (char '#')
                                (many $ satisfy (/= '#')))

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceAndComments

symbol :: String -> Parser String
symbol = lexeme . try . string

progP :: Parser Prog
progP = do
  spaceAndComments
  symbol "set $width "
  w <- intP
  symbol "set $height "
  h <- intP
  body <- expP
  return $ Prog w h body

expP :: Parser Exp
expP = lexeme (parExpP <|> specialP <|> letP <|> constP <|> listP
               <|> lengthP <|> seqP <|> allP <|> anyP <|> sumP <|> productP
               <|> mapP <|> reduceP <|> andP <|> orP <|> notP <|> addP
               <|> subtractP <|> multiplyP <|> moduloP <|> eqP <|> gtEqP
               <|> gtP <|> ltEqP <|> ltP <|> indexP <|> varP)

parExpP :: Parser Exp
parExpP = between (symbol "(") (symbol ")") expP

identP :: Parser String
identP = lexeme $ many1 (alphaNum <|> char '_')

funP :: Parser Fun
funP = lexeme $ between (symbol "(") (symbol ")") $ do
  symbol "\\"
  args <- many1 identP
  symbol "->"
  body <- expP
  return $ Fun args body

intP :: Parser Int
intP = lexeme (read <$> many1 digit)

specialP :: Parser Exp
specialP =
  char '$' >>
  ((symbol "xs" >> return BoardXs) <|>
   (symbol "ys" >> return BoardYs) <|>
   (symbol "vs" >> return BoardValues) <|>
   (symbol "width" >> return BoardWidth) <|>
   (symbol "height" >> return BoardHeight))

letP :: Parser Exp
letP = do
  symbol "let"
  v <- identP
  symbol "="
  e <- expP
  symbol "in"
  r <- expP
  return $ Let v e r

constP :: Parser Exp
constP = Const <$> intP

listP :: Parser Exp
listP = List <$> between (symbol "{") (symbol "}") (expP `sepBy` symbol ",")

lengthP :: Parser Exp
lengthP = do
  symbol "length"
  Length <$> expP

seqP :: Parser Exp
seqP = do
  symbol "seq"
  Seq <$> expP <*> expP

allP :: Parser Exp
allP = do
  symbol "all"
  All <$> expP

anyP :: Parser Exp
anyP = do
  symbol "any"
  Any <$> expP

sumP :: Parser Exp
sumP = do
  symbol "sum"
  Sum <$> expP

productP :: Parser Exp
productP = do
  symbol "product"
  Product <$> expP

mapP :: Parser Exp
mapP = do
  symbol "map"
  Map <$> funP <*> expP

reduceP :: Parser Exp
reduceP = do
  symbol "reduce"
  Reduce <$> funP <*> expP <*> expP

andP :: Parser Exp
andP = do
  symbol "and"
  And <$> expP <*> expP

orP :: Parser Exp
orP = do
  symbol "or"
  Or <$> expP <*> expP

notP :: Parser Exp
notP = do
  symbol "not"
  Not <$> expP

addP :: Parser Exp
addP = do
  symbol "+"
  Add <$> expP <*> expP

subtractP :: Parser Exp
subtractP = do
  symbol "-"
  Subtract <$> expP <*> expP

multiplyP :: Parser Exp
multiplyP = do
  symbol "*"
  Multiply <$> expP <*> expP

moduloP :: Parser Exp
moduloP = do
  symbol "%"
  Modulo <$> expP <*> expP

eqP :: Parser Exp
eqP = do
  symbol "=="
  Eq <$> expP <*> expP

gtEqP :: Parser Exp
gtEqP = try $ do
  symbol ">="
  GtEq <$> expP <*> expP

gtP :: Parser Exp
gtP = do
  symbol ">"
  Gt <$> expP <*> expP

ltEqP :: Parser Exp
ltEqP = try $ do
  symbol "<="
  LtEq <$> expP <*> expP

ltP :: Parser Exp
ltP = do
  symbol "<"
  Lt <$> expP <*> expP

indexP :: Parser Exp
indexP = between (symbol "[") (symbol "]") (Index <$> expP <*> expP)

varP :: Parser Exp
varP = Var <$> identP
