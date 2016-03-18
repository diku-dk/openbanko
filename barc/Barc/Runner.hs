module Barc.Runner (runString, runStringBit, runStringFuthark,
                    runStringFutharkUnrolled,
                    runFromFile, runFromFileBit, runFromFileFuthark,
                    runFromFileFutharkUnrolled) where

import Barc.Parser (parseString)
import Barc.Simplifier (simplify)
import Barc.Unroller (unroll)
import Barc.Bitter (bit)
import Barc.CCodeGen (genCode)
import Barc.CCodeGenBit (genCodeBit)
import Barc.FutharkGen (genFuthark)
import Barc.FutharkUnrolledGen (genFutharkUnrolled)

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

runStringBit :: String -> Either String T.Text
runStringBit s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> case simplify p0 of
    Left err -> Left ("simplify error: " ++ err)
    Right p1 -> case unroll p1 of
      Left err -> Left ("unroll error: " ++ err)
      Right p2 -> Right $ genCodeBit (bit p2)

runStringFuthark :: String -> Either String T.Text
runStringFuthark s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> case simplify p0 of
    Left err -> Left ("simplify error: " ++ err)
    Right p1 -> Right $ genFuthark p1

runStringFutharkUnrolled :: String -> Either String T.Text
runStringFutharkUnrolled s = case parseString s of
  Left err -> Left ("parse error: " ++ show err)
  Right p0 -> case simplify p0 of
    Left err -> Left ("simplify error: " ++ err)
    Right p1 -> case unroll p1 of
      Left err -> Left ("unroll error: " ++ err)
      Right p2 -> Right $ genFutharkUnrolled p2

runFromFile :: FilePath -> IO (Either String T.Text)
runFromFile path = runString <$> readFile path

runFromFileBit :: FilePath -> IO (Either String T.Text)
runFromFileBit path = runStringBit <$> readFile path

runFromFileFuthark :: FilePath -> IO (Either String T.Text)
runFromFileFuthark path = runStringFuthark <$> readFile path

runFromFileFutharkUnrolled :: FilePath -> IO (Either String T.Text)
runFromFileFutharkUnrolled path = runStringFutharkUnrolled <$> readFile path
