{-# LANGUAGE TupleSections #-}
module Bankotrav.Base where

import Data.List (foldl', transpose, findIndex)

import Bankotrav.Types


minimumCellValue :: Int -> Int
minimumCellValue column = column * 10 + if column == 0 then 1 else 0

maximumCellValue :: Int -> Int
maximumCellValue column = column * 10 + 9 + if column == 8 then 1 else 0

validCellsRaw :: Int -> [Cell]
validCellsRaw column = BlankCell : map (ValueCell . (10 * column +)) [start..end]
  where start = if column == 0 then 1 else 0
        end  = if column == 8 then 10 else 9

-- Assumes not being called on a filled-in cell
validCells :: BoardIncomplete -> CellIndex -> [Cell]
validCells board index@(column, row) = filter alright $ validCellsRaw column
  where alright cell =
          let board' = setCell board index $ FilledIn cell
          in (getCell board (column, row - 2)) `isLowerOrBlank` (Just (FilledIn cell)) &&
             (getCell board (column, row - 1)) `isLowerOrBlank` (Just (FilledIn cell)) &&
             (Just (FilledIn cell)) `isLowerOrBlank` (getCell board (column, row + 1)) &&
             (Just (FilledIn cell)) `isLowerOrBlank` (getCell board (column, row + 2)) &&
             columnsCanEndWell board'

isLowerOrBlank :: (Maybe CellIncomplete) -> (Maybe CellIncomplete) -> Bool
isLowerOrBlank a b = case (a, b) of
  (Just (FilledIn (ValueCell va)), Just (FilledIn (ValueCell vb))) -> va < vb
  _ -> True


validColumnSignatures :: [ColumnSignature]
validColumnSignatures = [ (3, 0, 6)
                        , (2, 2, 5)
                        , (1, 4, 4)
                        , (0, 6, 3)
                        ]

kindPerms :: ColumnBoardKind -> [ColumnBoardPerm]
kindPerms = filter valid . perms . map columnKindPerms
  where columnKindPerms kind = case kind of
          ThreeCells -> [ (Number, Number, Number) ]
          TwoCells -> [ (Number, Number, Blank)
                      , (Number, Blank, Number)
                      , (Blank, Number, Number)
                      ]
          OneCell -> [ (Number, Blank, Blank)
                     , (Blank, Number, Blank)
                     , (Blank, Blank, Number)
                     ]

        perms :: [[ColumnPerm]] -> [ColumnBoardPerm]
        perms (pos : poss) = concatMap (\t -> map (t :) $ perms poss) pos
        perms [] = [[]]

        valid :: ColumnBoardPerm -> Bool
        valid = all ((== 5) . length . filter isNumber) . transpose . map (\(a, b, c) -> [a, b, c])

columnSignaturePermutations :: ColumnSignature -> [ColumnBoardKind]
columnSignaturePermutations (threes, twos, ones) =
  threes_results ++ twos_results ++ ones_results ++ end
  where threes_results =
          if threes > 0
          then map (ThreeCells :) $ columnSignaturePermutations (threes - 1, twos, ones)
          else []
        twos_results =
          if twos > 0
          then map (TwoCells :) $ columnSignaturePermutations (threes, twos - 1, ones)
          else []
        ones_results =
          if ones > 0
          then map (OneCell :) $ columnSignaturePermutations (threes, twos, ones - 1)
          else []
        end =
          if threes == 0 && twos == 0 && ones == 0
          then [[]]
          else []

-- This is actually only 1554.
allColumnSignaturePermutations :: [ColumnBoardKind]
allColumnSignaturePermutations = concatMap columnSignaturePermutations validColumnSignatures

columnsCanEndWell :: BoardIncomplete -> Bool
columnsCanEndWell board = any (boardColumnsCanEndInKind board) allColumnSignaturePermutations


minim column = Just $ FilledIn $ ValueCell $ minimumCellValue column
maxim column = Just $ FilledIn $ ValueCell $ maximumCellValue column

okays column a b c =
  let c' = case c of
        FilledIn (ValueCell k) -> FilledIn (ValueCell (k - 1))
        _ -> c

      a_okay =
        not (isFilledInBlank a) && isFilledIn a || (not (isFilledInBlank a) &&
                         minim column `isLowerOrBlank` Just b &&
                         minim column `isLowerOrBlank` Just c)
      b_okay = not (isFilledInBlank b) && isFilledIn b || (not (isFilledInBlank b) &&
                                Just a `isLowerOrBlank` Just c' &&
                                minim column `isLowerOrBlank` Just c &&
                                Just a `isLowerOrBlank` maxim column)
      c_okay = not (isFilledInBlank c) && isFilledIn c || (not (isFilledInBlank c) &&
                                Just b `isLowerOrBlank` maxim column &&
                                Just a `isLowerOrBlank` maxim column)
  in (a_okay, b_okay, c_okay)

columnPermCanWork :: BoardIncomplete -> ColumnBoardPerm -> Bool
columnPermCanWork board perm = and $ zipWith3 (columnColumnPermCanWork board) [0..8] perm (map (getColumn board) [0..8])

columnColumnPermCanWork :: BoardIncomplete -> Int -> ColumnPerm -> Column CellIncomplete -> Bool
columnColumnPermCanWork board column (ka, kb, kc) (a, b, c) =
  let (a_okay, b_okay, c_okay) = okays column a b c
  in ok ka a a_okay && ok kb b b_okay && ok kc c c_okay
  where ok Number _ o = o
        ok Blank t _ = case t of
          FilledIn BlankCell -> True
          NotFilledIn -> True -- ?
          _ -> False

boardColumnsCanEndInKind :: BoardIncomplete -> ColumnBoardKind -> Bool
boardColumnsCanEndInKind board kind = okayColumnWise && okayRowWise
  where okayRowWise = any (columnPermCanWork board) $ kindPerms kind

        okayColumnWise = and $ zipWith3 columnCanEndInKind [0..8] (map (getColumn board) [0..8]) kind

        columnCanEndInKind :: Int -> Column CellIncomplete -> ColumnKind -> Bool
        columnCanEndInKind column (a, b, c) ckind =
          let (a_okay, b_okay, c_okay) = okays column a b c
              (a_notnum, b_notnum, c_notnum) = (not $ hasValue a, not $ hasValue b, not $ hasValue c)
          in case ckind of
            ThreeCells ->
              a_okay && b_okay && c_okay
            TwoCells ->
              (a_okay && b_okay && c_notnum) ||
              (a_okay && c_okay && b_notnum) ||
              (b_okay && c_okay && a_notnum)
            OneCell ->
              (a_okay && b_notnum && c_notnum) ||
              (b_okay && a_notnum && c_notnum) ||
              (c_okay && a_notnum && b_notnum)

emptyBoard :: BoardIncomplete
emptyBoard = replicate 9 $ replicate 3 NotFilledIn


boardIndices :: [CellIndex]
boardIndices = concatMap (\c -> map (c, ) [0..2]) [0..8]
