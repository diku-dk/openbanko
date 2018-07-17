module Bankotrav.Types where

import Safe (atMay)


data Cell = BlankCell
          | ValueCell Int
  deriving (Show, Eq)

data CellIncomplete = NotFilledIn
                    | FilledIn Cell
  deriving (Show)

hasValue :: CellIncomplete -> Bool
hasValue NotFilledIn = False
hasValue (FilledIn BlankCell) = False
hasValue _ = True

type CellIndex = (Int, Int)

type BoardBase cell = [[cell]] -- 9 columns * 3 rows
type Board = BoardBase Cell -- Complete board
type BoardIncomplete = BoardBase CellIncomplete -- Board in creation

getCell :: BoardBase cell -> CellIndex -> Maybe cell
getCell board (column, row) = do
  elems <- atMay board column
  atMay elems row

setCell :: BoardBase cell -> CellIndex -> cell -> BoardBase cell
setCell board (column, row) cell =
  let elems = board !! column
      elems' = take row elems ++ [cell] ++ drop (row + 1) elems
  in take column board ++
     [elems'] ++
     drop (column + 1) board

type Column cell = (cell, cell, cell)

getColumn :: BoardBase cell -> Int -> Column cell
getColumn board i = (col !! 0, col !! 1, col !! 2)
  where col = board !! i

isFilledIn :: CellIncomplete -> Bool
isFilledIn NotFilledIn = False
isFilledIn _ = True

isFilledInBlank :: CellIncomplete -> Bool
isFilledInBlank (FilledIn BlankCell) = True
isFilledInBlank _ = False




data ColumnKind = ThreeCells | TwoCells | OneCell
  deriving (Show, Eq)

type ColumnBoardKind = [ColumnKind] -- 9 columns

data CellKind = Number | Blank
  deriving (Show)

isNumber :: CellKind -> Bool
isNumber Number = True
isNumber Blank = False


type ColumnSignature = (Int, Int, Int)



type ColumnPerm = (CellKind, CellKind, CellKind)
type ColumnBoardPerm = [ColumnPerm] -- 9 columns



fromIncomplete :: BoardIncomplete -> Board
fromIncomplete = map (map from')
  where from' (FilledIn v) = v
        from' NotFilledIn = error "not fully filled in"
