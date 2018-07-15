{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- random, random-extras, random-fu

module Random (
  CMR.Random,
  CMR.MonadRandom,
  CMR.evalRand,
  CMR.evalRandIO,
  CMR.evalRandT,
  CMR.runRandT,
  CMR.runRand,
  runRandTIO,
  evalRandTIO,
  CMR.mkStdGen,
  RandomState,
  RandomStateT,
  randomSt,
  randomR,
  choice,
  choiceMay,
  choiceFromType,
  weightedChoice,
  weightedChoiceMay,
  sample,
  shuffle
  , randomTest
  ) where
import Control.Monad
import Data.Functor ((<$>))
import qualified Control.Monad.Random as CMR
import qualified Data.Random as DR
import qualified Data.Random.Extras as DRE
import Data.Random.Distribution.Uniform
import Data.Word (Word64)

type RandomState a = CMR.Rand CMR.StdGen a
type RandomStateT m a = CMR.RandT CMR.StdGen m a

-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Random number in range @a..b@.
randomR :: (CMR.Random a, CMR.MonadRandom m) => a -> a -> m a
randomR a b = CMR.getRandomR (a, b)

-- | Random element from list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = randomSt . DRE.choice

-- | Nothing if empty list.
choiceMay :: CMR.MonadRandom m => [a] -> Maybe (m a)
choiceMay = liftM randomSt . DRE.safeChoice

-- | Random element from weighted list.
weightedChoice :: CMR.MonadRandom m => [(a, Rational)] -> m a
weightedChoice = CMR.fromList

weightedChoiceMay :: CMR.MonadRandom m => [(a, Rational)] -> Maybe (m a)
weightedChoiceMay [] = Nothing
weightedChoiceMay xs = Just (CMR.fromList xs)

-- | Chooses a constructor from the type.
choiceFromType :: (Monad m, Enum a, Bounded a) => RandomStateT m a
choiceFromType = choice $ enumFrom minBound

-- | Random sample from list.
sample :: CMR.MonadRandom m => Int -> [a] -> m [a]
sample n = randomSt . DRE.sample n

-- | Shuffles list.
shuffle :: CMR.MonadRandom m => [a] -> m [a]
shuffle = randomSt . DRE.shuffle

-- | Runs in IO.
runRandTIO :: Monad m => RandomStateT m a -> IO (m (a, CMR.StdGen))
runRandTIO x = liftM (CMR.runRandT x) CMR.newStdGen

-- | Evaluates in IO.
evalRandTIO :: Monad m => RandomStateT m a -> IO (m a)
evalRandTIO x = liftM (CMR.evalRandT x) CMR.newStdGen

-- | Test and print.
randomTest :: Show a => Int -> RandomState a -> IO ()
randomTest n m = mapM_ print =<< CMR.evalRandIO (replicateM n m)
