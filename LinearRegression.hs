module LinearRegression(newWeights) where

import Data.List 
import RandomSampling

-- Calculate new values for t0 and t1
-- newWeights :: Coefficients -> LerningRate -> Data -> Coefficients
newWeights :: [Double] -> Double -> [[Double]] -> IO [Double]
newWeights weights alpha dataset = do
    batch <- sampleDataUnique (length dataset `div` 2) dataset 
    let deltas = map (calculateDelta weights) batch
        adjustedDeltas = adjustDeltas deltas batch  
        newW0 = w0 - alpha * avg deltas 
        newW1 = w1 - alpha * avg adjustedDeltas
    return [newW0, newW1]
    where 
        w0 = weights !! 0
        w1 = weights !! 1
    
-- Calculates the difference between h(x) and y
calculateDelta :: [Double] -> [Double] -> Double
calculateDelta weights row = w0 + w1 * x - y
  where
    w0 = weights !! 0
    w1 = weights !! 1
    x = row !! 0
    y = row !! 1

adjustDeltas :: [Double] -> [[Double]] -> [Double]
adjustDeltas deltas dataset = 
    zipWith (*) deltas features
    where 
        features = map head dataset

avg xs = realToFrac (sum xs) / genericLength xs