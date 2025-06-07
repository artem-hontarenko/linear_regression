module LinearRegression(newWeights) where

import Data.List 
import RandomSampling

-- Calculate new values for weights
newWeights :: [Double] -> Double -> [[Double]] -> IO [Double]
newWeights weights alpha dataset = do
    batch <- sampleDataUnique (length dataset `div` 2) dataset 
    let deltas = map (calculateDelta weights) batch
        numFeatures = length weights - 1
        adjustedDeltas = [zipWith(*) deltas (ithFeatureColumn i batch) | i <- [0..numFeatures - 1]]
        newW0 = head weights - alpha * avg deltas
        newW1_Wn = [weights !! (i+1) - alpha * avg(adjustedDeltas !! i) | i <- [0..numFeatures - 1]]
    return (newW0 : newW1_Wn)

-- Calculate difference between prediction and actual target
calculateDelta :: [Double] -> [Double] -> Double
calculateDelta weights row = predict_y - y
    where 
        y = last row
        features = init row
        w0 = head weights
        predict_y = w0 + sum (zipWith (*) (tail weights) features)

-- Get the column consists of i-th values from every entry in the batch 
ithFeatureColumn :: Int -> [[Double]] -> [Double]
ithFeatureColumn columnID = map (!! columnID)   

-- Compute average value for the list
avg xs = realToFrac (sum xs) / genericLength xs
