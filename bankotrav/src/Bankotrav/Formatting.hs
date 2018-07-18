module Bankotrav.Formatting where

import Data.List (transpose)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Bankotrav.Types
import Bankotrav.Base


formatBoard :: Board -> String
formatBoard = unlines . map (unwords . map formatCell) . transpose . boardToList
  where formatCell BlankCell = "00"
        formatCell (ValueCell v) = (if length (show v) == 1 then "0" else "") ++ show v

formatBinary :: Int -> String
formatBinary i = showIntAtBase 2 intToDigit i ""
