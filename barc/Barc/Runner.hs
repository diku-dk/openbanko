module Barc.Runner (runStringFuthark, runFromFile, runFromFileFuthark) where

import Control.Applicative

import Barc.Parser (parseString)
import Barc.Simplifier (simplify)
import Barc.Unroller (unroll)
import Barc.CodeGen (genCode)
import Barc.FutharkGen (genFuthark)

import Prelude

runString :: String -> Either String String
runString s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> case simplify p0 of
    Left err -> Left ("simplify error: " ++ err)
    Right p1 -> case unroll p1 of
      Left err -> Left ("unroll error: " ++ err)
      Right p2 -> Right $ genCode p2

runStringFuthark :: String -> Either String String
runStringFuthark s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> genFuthark p0

runFromFile :: FilePath -> IO (Either String String)
runFromFile path = runString <$> readFile path

runFromFileFuthark :: FilePath -> IO (Either String String)
runFromFileFuthark path = runStringFuthark <$> readFile path
