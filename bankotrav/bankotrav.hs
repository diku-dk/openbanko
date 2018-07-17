{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
-- bankotrav: traverse banko boards
module Main where

import Data.List (foldl', transpose, findIndex)
import Data.Maybe (fromMaybe)
import Safe (atMay)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import qualified Data.Random as DR
import qualified Control.Monad.Random as CMR
import qualified Data.Random.Extras as DRE
import Data.Word (Word64)

import System.IO.Unsafe (unsafePerformIO)


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


type ColumnSignature = (Int, Int, Int)

validColumnSignatures :: [ColumnSignature]
validColumnSignatures = [ (3, 0, 6)
                        , (2, 2, 5)
                        , (1, 4, 4)
                        , (0, 6, 3)
                        ]

data ColumnKind = ThreeCells | TwoCells | OneCell
  deriving (Show, Eq)

type ColumnBoardKind = [ColumnKind] -- 9 columns

data CellKind = Number | Blank
  deriving (Show)

isNumber :: CellKind -> Bool
isNumber Number = True
isNumber Blank = False

type ColumnPerm = (CellKind, CellKind, CellKind)
type ColumnBoardPerm = [ColumnPerm] -- 9 columns

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

fromIncomplete :: BoardIncomplete -> Board
fromIncomplete = map (map from')
  where from' (FilledIn v) = v
        from' NotFilledIn = error "not fully filled in"

type Random a = CMR.Rand CMR.StdGen a

-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Shuffles list.
shuffle :: CMR.MonadRandom m => [a] -> m [a]
shuffle = randomSt . DRE.shuffle

-- | Random element from list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = randomSt . DRE.choice

randomBoard :: Random Board
randomBoard = do
  indices <- shuffle $ concatMap (\c -> map (c, ) [0..2]) [0..8]
  bi <- step emptyBoard indices
  return $ fromIncomplete bi

  where step :: BoardIncomplete -> [CellIndex] -> Random BoardIncomplete
        step b [] = return b
        step b (i : is) = do
          let cs = validCells b i
          c <- choice cs
          let b' = setCell b i $ FilledIn c
          step b' is

randomBoardIO :: IO Board
randomBoardIO = CMR.evalRandIO randomBoard

formatBoard :: Board -> String
formatBoard = unlines . map (unwords . map cellFormat) . transpose
  where cellFormat BlankCell = "00"
        cellFormat (ValueCell v) = (if length (show v) == 1 then "0" else "") ++ show v


boardIndices :: [CellIndex]
boardIndices = concatMap (\c -> map (c, ) [0..2]) [0..8]

nPossibleBoards :: BoardIncomplete -> Int
nPossibleBoards bi = sum $ map poss perms
  where perms = filter (columnPermCanWork bi) $ concatMap kindPerms allColumnSignaturePermutations
        poss :: ColumnBoardPerm -> Int
        poss perm = product $ zipWith colPoss perm [0..8]
        colPoss :: ColumnPerm -> Int -> Int
        colPoss (ka, kb, kc) col =
          let (a, b, c) = getColumn bi col
          in length $ work (minimumCellValue col - 1) (zip [a, b, c] [ka, kb, kc])
          where work _ [] = [[]]
                work prev ((t, tk) : us) = case tk of
                  Blank -> work prev us
                  Number -> case t of
                    FilledIn (ValueCell v) -> work v us
                    _ -> concatMap (\t -> work t us) [(prev + 1)..(maxk us)]

                maxk [] = maximumCellValue col
                maxk ((FilledIn (ValueCell t), _) : _) = t - 1
                maxk (_ : us) = maxk us


boardIndex :: Board -> Int
boardIndex board = fst $ foldl' step (0, emptyBoard) boardIndices
  where step (acc, bi) i = fromMaybe (error "impossible!") $ do
          cell <- getCell board i
          let choices = validCells bi i
          choice_i <- findIndex (== cell) choices
          let bi' = setCell bi i $ FilledIn cell
              acc' = acc + sum (map (nPossibleBoards . setCell bi i . FilledIn) (take choice_i choices))
          unsafePerformIO (print ("compress", i, acc, acc')) `seq` return (acc', bi')

indexToBoard :: Int -> Board
indexToBoard idx = fromIncomplete $ snd $ foldl' step (idx, emptyBoard) boardIndices
  where step (acc, bi) i =
          let choices = validCells bi i
              (choice, prev_sum) = find_choice 0 0 choices
              acc' = acc - prev_sum
              bi' = setCell bi i $ FilledIn choice
          in unsafePerformIO (print ("decompress", i, acc, acc')) `seq` (acc', bi')

          where find_choice cur_choice_i cur (choice : choices) =
                  let new = nPossibleBoards $ setCell bi i $ FilledIn choice
                      cur' = cur + new
                  in if cur' > acc
                  then (choice, cur)
                  else find_choice (cur_choice_i + 1) cur' choices
                find_choice _ _ [] = error "impossible!"


main :: IO ()
--main = putStr =<< formatBoard <$> randomBoardIO
main = do
  putStrLn "Generating random board..."
  board <- randomBoardIO
  putStr $ formatBoard board
  putStrLn "Compressing board..."
  let idx = boardIndex board
  print idx
  putStrLn $ showIntAtBase 2 intToDigit idx ""
  putStrLn "Decompressing board..."
  let board' = indexToBoard idx
  putStr $ formatBoard board'

  --print $ nPossibleBoards board
--  where -- board = setCell emptyBoard (0, 0) (FilledIn (ValueCell 3))
  --      board = emptyBoard
