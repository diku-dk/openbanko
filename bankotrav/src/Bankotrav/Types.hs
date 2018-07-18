module Bankotrav.Types where

import qualified Data.Array.IArray as A
import Data.Array.IArray ((!), (//))


data Cell = BlankCell
          | ValueCell !Int
  deriving (Show, Eq)

data CellIncomplete = NotFilledIn
                    | FilledIn !Cell
  deriving (Show)

type CellIndex = (Int, Int)
type BoardBase cell = A.Array CellIndex cell -- 9 columns * 3 rows

type Board = BoardBase Cell -- Complete board
type BoardIncomplete = BoardBase CellIncomplete -- Board in creation

type Column cell = (cell, cell, cell) -- Nicer representation for single columns

data ColumnKind = ThreeCells | TwoCells | OneCell
  deriving (Show, Eq)

type BoardKind = [ColumnKind] -- 9 columns

-- | (three-number columns, two-number columns, one-number columns)
type ColumnSignature = (Int, Int, Int)

data CellType = Number | Blank
  deriving (Show)

type ColumnPerm = (CellType, CellType, CellType)
type BoardPerm = [ColumnPerm] -- 9 columns


getCell :: BoardBase cell -> CellIndex -> cell
getCell = (!)

getCellMay :: BoardBase cell -> CellIndex -> Maybe cell
getCellMay board i =
  let (start, end) = A.bounds board
  in if i < start || i > end
     then Nothing
     else Just $ getCell board i

setCell :: BoardIncomplete -> CellIndex -> Cell -> BoardIncomplete
setCell board i cell = setCell' board i $ FilledIn cell

setCell' :: BoardBase cell -> CellIndex -> cell -> BoardBase cell
setCell' board i cell = board // [(i, cell)]

getColumn :: BoardBase cell -> Int -> Column cell
getColumn board i = (board ! (i, 0), board ! (i, 1), board ! (i, 2))

isFilledIn :: CellIncomplete -> Bool
isFilledIn (FilledIn (ValueCell _)) = True
isFilledIn _ = False

isFilledIn' :: CellIncomplete -> Bool
isFilledIn' (FilledIn _) = True
isFilledIn' _ = False

isBlank :: CellIncomplete -> Bool
isBlank (FilledIn BlankCell) = True
isBlank _ = False

typeIsNumber :: CellType -> Bool
typeIsNumber Number = True
typeIsNumber Blank = False
