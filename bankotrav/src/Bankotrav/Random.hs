{-# LANGUAGE ScopedTypeVariables #-}
module Bankotrav.Random (
  randomBoard, randomBoardIO,
  randomBoardSimple, randomBoardSimpleIO
  ) where

import Control.Monad (foldM)
import qualified Data.Random as DR
import qualified Data.Random.List as DRL
import qualified Control.Monad.Random as CMR
import Data.Word (Word64)

import Bankotrav.Types
import Bankotrav.Base
import Bankotrav.Counting
import Bankotrav.Compression


type Random a = CMR.Rand CMR.StdGen a

-- | Makes a function that returns a RVar usable by e.g. CMR.Rand.
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
  indices <- shuffle boardIndices
  completeBoard <$> foldM step emptyBoard indices
  where step bi i = do
          c <- choice $ validCells bi i
          return $ setCell bi i c

randomBoardIO :: IO Board
randomBoardIO = CMR.evalRandIO randomBoard


randomBoardSimple :: Random Board
randomBoardSimple = decompressBoard <$> CMR.getRandomR (0, nPossibleBoards emptyBoard)

randomBoardSimpleIO :: IO Board
randomBoardSimpleIO = CMR.evalRandIO randomBoardSimple
