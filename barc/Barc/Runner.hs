module Barc.Runner (runStringFuthark, runFromFile, runFromFileFuthark) where

import Barc.Parser (parseString)
import Barc.Simplifier (simplify)
import Barc.Unroller (unroll)
import Barc.CCodeGen (genCode)
import Barc.FutharkGen (genFuthark)

import Control.Applicative
import Prelude
import qualified Data.Text as T


runString :: String -> Either String T.Text
runString s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> case simplify p0 of
    Left err -> Left ("simplify error: " ++ err)
    Right p1 -> case unroll p1 of
      Left err -> Left ("unroll error: " ++ err)
      Right p2 -> Right $ genCode p2

runStringFuthark :: String -> Either String T.Text
runStringFuthark s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> T.pack <$> genFuthark p0

runFromFile :: FilePath -> IO (Either String T.Text)
runFromFile path = runString <$> readFile path

runFromFileFuthark :: FilePath -> IO (Either String T.Text)
runFromFileFuthark path = runStringFuthark <$> readFile path
