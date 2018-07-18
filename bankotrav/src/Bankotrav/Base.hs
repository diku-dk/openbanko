{-# LANGUAGE TupleSections #-}
module Bankotrav.Base where

import Data.List (transpose, groupBy)
import qualified Data.Array.IArray as A

import Bankotrav.Types


minimumCellValue :: Int -> Int
minimumCellValue column = column * 10 + if column == 0 then 1 else 0

maximumCellValue :: Int -> Int
maximumCellValue column = column * 10 + 9 + if column == 8 then 1 else 0

isLowerOrBlank :: Maybe CellIncomplete -> Maybe CellIncomplete -> Bool
isLowerOrBlank a b = case (a, b) of
  (Just (FilledIn (ValueCell va)),
   Just (FilledIn (ValueCell vb))) ->
    va < vb
  _ ->
    True

validCellsRaw :: Int -> [Cell]
validCellsRaw column = BlankCell : map ValueCell [minimumCellValue column..maximumCellValue column]

-- Assumes not being called on a filled-in cell.
validCells :: BoardIncomplete -> CellIndex -> [Cell]
validCells board i@(column, row) = filter valid $ validCellsRaw column
  where valid cell =
          -- Rough checks.
          getCellMay board (column, max 0 (row - 2)) `isLowerOrBlank` Just (FilledIn cell) &&
          getCellMay board (column, max 0 (row - 1)) `isLowerOrBlank` Just (FilledIn cell) &&
          Just (FilledIn cell) `isLowerOrBlank` getCellMay board (column, min 2 (row + 1)) &&
          Just (FilledIn cell) `isLowerOrBlank` getCellMay board (column, min 2 (row + 2)) &&
          -- Complete checks.
          boardCanBeFilled (setCell board i cell)

-- Explained in Nils Andersen's paper.
validColumnSignatures :: [ColumnSignature]
validColumnSignatures = [ (3, 0, 6)
                        , (2, 2, 5)
                        , (1, 4, 4)
                        , (0, 6, 3)
                        ]

boardKindPermutations :: BoardKind -> [BoardPerm]
boardKindPermutations = filter valid . perms . map columnKindPermutations
  where perms :: [[ColumnPerm]] -> [BoardPerm]
        perms (pos : poss) = concatMap (\t -> map (t :) $ perms poss) pos
        perms [] = [[]]

        valid :: BoardPerm -> Bool
        valid = all ((== 5) . length . filter typeIsNumber) . transpose .
                map (\(a, b, c) -> [a, b, c])

columnKindPermutations :: ColumnKind -> [ColumnPerm]
columnKindPermutations kind = case kind of
  ThreeCells -> [ (Number, Number, Number) ]
  TwoCells -> [ (Number, Number, Blank)
              , (Number, Blank, Number)
              , (Blank, Number, Number)
              ]
  OneCell -> [ (Number, Blank, Blank)
             , (Blank, Number, Blank)
             , (Blank, Blank, Number)
             ]

boardKinds :: [BoardKind]
boardKinds = concatMap signatureBoardKinds validColumnSignatures

signatureBoardKinds :: ColumnSignature -> [BoardKind]
signatureBoardKinds (threes, twos, ones) =
  check threes ThreeCells (threes - 1, twos, ones) ++
  check twos TwoCells (threes, twos - 1, ones) ++
  check ones OneCell (threes, twos, ones - 1) ++
  check_end
  where check n kind acc
          | n > 0 = map (kind :) $ signatureBoardKinds acc
          | otherwise = []
        check_end
          | threes == 0 && twos == 0 && ones == 0 = [[]]
          | otherwise = []

boardCanBeFilled :: BoardIncomplete -> Bool
boardCanBeFilled board = any (boardCanEnd board) boardKinds

-- Can an incomplete board be completed into a valid board?
boardCanEnd :: BoardIncomplete -> BoardKind -> Bool
boardCanEnd board kind =
  boardCanEndInKind board kind && -- Do a quick check.
  any (boardCanEndInPerm board) (boardKindPermutations kind) -- Check properly.

boardCanEndInKind :: BoardIncomplete -> BoardKind -> Bool
boardCanEndInKind board kind =
  and $ zipWith3 columnCanEndInKind [0..8] (map (getColumn board) [0..8]) kind

columnCanEndInKind :: Int -> Column CellIncomplete -> ColumnKind -> Bool
columnCanEndInKind column cells@(a, b, c) ckind =
  let (a_valid, b_valid, c_valid) = columnCellsValid column cells
      (a_notnum, b_notnum, c_notnum) =
        (not $ isFilledIn a, not $ isFilledIn b, not $ isFilledIn c)
  in case ckind of
       ThreeCells ->
         a_valid && b_valid && c_valid
       TwoCells ->
         (a_valid && b_valid && c_notnum) ||
         (a_valid && c_valid && b_notnum) ||
         (b_valid && c_valid && a_notnum)
       OneCell ->
         (a_valid && b_notnum && c_notnum) ||
         (b_valid && a_notnum && c_notnum) ||
         (c_valid && a_notnum && b_notnum)

boardCanEndInPerm :: BoardIncomplete -> BoardPerm -> Bool
boardCanEndInPerm board perm =
  and $ zipWith3 columnCanEndInPerm [0..8] perm (map (getColumn board) [0..8])

columnCanEndInPerm :: Int -> ColumnPerm -> Column CellIncomplete -> Bool
columnCanEndInPerm column (ka, kb, kc) cells@(a, b, c) =
  let (a_valid, b_valid, c_valid) = columnCellsValid column cells
  in ok ka a a_valid && ok kb b b_valid && ok kc c c_valid
  where ok Number _ o = o
        ok Blank t _ = case t of
          FilledIn (ValueCell _) -> False
          _ -> True

columnCellsValid :: Int -> Column CellIncomplete -> (Bool, Bool, Bool)
columnCellsValid column (a, b, c) =
  let c' = case c of
        FilledIn (ValueCell k) -> FilledIn (ValueCell (k - 1))
        _ -> c
      a_valid = isFilledIn a || (not (isBlank a) &&
                                 minim column `isLowerOrBlank` Just b &&
                                 minim column `isLowerOrBlank` Just c)
      b_valid = isFilledIn b || (not (isBlank b) &&
                                 Just a `isLowerOrBlank` Just c' &&
                                 minim column `isLowerOrBlank` Just c &&
                                 Just a `isLowerOrBlank` maxim column)
      c_valid = isFilledIn c || (not (isBlank c) &&
                                 Just b `isLowerOrBlank` maxim column &&
                                 Just a `isLowerOrBlank` maxim column)
  in (a_valid, b_valid, c_valid)
  where minim = Just . FilledIn . ValueCell . minimumCellValue
        maxim = Just . FilledIn . ValueCell . maximumCellValue


boardIndices :: [CellIndex]
boardIndices = concatMap (\c -> map (c, ) [0..2]) [0..8]

emptyBoard :: BoardIncomplete
emptyBoard = A.listArray ((0, 0), (8, 2)) (replicate (9 * 3) NotFilledIn)

completeBoard :: BoardIncomplete -> Board
completeBoard = A.amap extract
  where extract (FilledIn v) = v
        extract NotFilledIn = error "not fully filled in"

boardToList :: BoardBase cell -> [[cell]]
boardToList = map (map snd) .
              groupBy (\((col0, _), _) ((col1, _), _) -> col0 == col1) .
              A.assocs
