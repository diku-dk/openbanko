module Bankotrav.Counting (nPossibleBoards) where

import Bankotrav.Types
import Bankotrav.Base


nPossibleBoards :: BoardIncomplete -> Int
nPossibleBoards bi = sum $ map boardPermPossibilities perms
  where perms = filter (boardCanEndInPerm bi)
                $ concatMap boardKindPermutations
                $ filter (boardCanEndInKind bi) boardKinds

        boardPermPossibilities perm =
          product $ zipWith columnPermPossibilities perm [0..8]

        columnPermPossibilities (ka, kb, kc) col =
          let (a, b, c) = getColumn bi col
              (cs, ks) = ([a, b, c], [ka, kb, kc])
              maxvals = scanr maxVal (maximumCellValue col) cs
          in calc (minimumCellValue col - 1) (zip3 cs ks maxvals)
          where maxVal (FilledIn (ValueCell t)) _ = t - 1
                maxVal _ acc = acc

                calc _ [] = 1
                calc prev ((t, tk, m) : zs) = case tk of
                  Blank -> calc prev zs
                  Number -> case t of
                    FilledIn (ValueCell v) -> calc v zs
                    _ -> sum $ map (`calc` zs) [prev + 1..m]
