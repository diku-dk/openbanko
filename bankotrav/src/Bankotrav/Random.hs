{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bankotrav.Random (randomBoard, randomBoardIO) where

import qualified Data.Random as DR
import qualified Data.Random.List as DRL
import qualified Control.Monad.Random as CMR
import Data.Word (Word64)

import Bankotrav.Types
import Bankotrav.Base

type Random a = CMR.Rand CMR.StdGen a


-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Shuffles list.
shuffle :: CMR.MonadRandom m => [a] -> m [a]
shuffle = randomSt . DRL.shuffle

-- | Random element from list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = randomSt . DRL.randomElement




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
