module RandomSampling where

import System.Random
import Data.List

-- Randomly sample n rows from dataset (without replacement)
sampleDataUnique :: Int -> [[Double]] -> IO [[Double]]
sampleDataUnique n dataset
    | n <= 0 = return []
    | n >= length dataset = return dataset
    | otherwise = do
        gen <- getStdGen
        sampleDataUnique' n dataset [] gen
  where
    sampleDataUnique' 0 _ acc _ = return acc
    sampleDataUnique' _ [] acc _ = return acc
    sampleDataUnique' count remaining acc gen = do
        let (index, newGen) = randomR (0, length remaining - 1) gen
            selected = remaining !! index
            newRemaining = take index remaining ++ drop (index + 1) remaining
        sampleDataUnique' (count - 1) newRemaining (selected : acc) newGen