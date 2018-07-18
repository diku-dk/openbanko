module Bankotrav.Compression where

import Data.List (foldl', elemIndex)
import Data.Maybe (fromMaybe)

import Bankotrav.Types
import Bankotrav.Base
import Bankotrav.Counting


compressBoard :: Board -> Int
compressBoard board = fst $ foldl' step (0, emptyBoard) boardIndices
  where step (acc, bi) i =
          let cell = getCell board i
              choices = validCells bi i
              choice_i = fromMaybe (error "impossible") $ elemIndex cell choices
              bi' = setCell bi i cell
              acc' = acc + sum (map (nPossibleBoards . setCell bi i) (take choice_i choices))
          in (acc', bi')

decompressBoard :: Int -> Board
decompressBoard idx = completeBoard $ snd $ foldl' step (idx, emptyBoard) boardIndices
  where step (acc, bi) i =
          let choices = validCells bi i
              (choice, prev_sum) = findChoice 0 choices
              acc' = acc - prev_sum
              bi' = setCell bi i choice
          in (acc', bi')

          where findChoice cur (choice : choices) =
                  let new = nPossibleBoards $ setCell bi i choice
                      cur' = cur + new
                  in if cur' > acc
                     then (choice, cur)
                     else findChoice cur' choices
                findChoice _ [] = error "impossible"
