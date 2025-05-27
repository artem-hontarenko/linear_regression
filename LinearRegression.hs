module LinearRegression(newThetas) where

import Data.List 

-- Finds coefficients for a linear regression using gradient descent
-- linearRegression :: Coefficients -> LerningRate -> Data -> Iterations -> Coefficients
linearRegression :: [Double] -> Double -> [[Double]] -> Int -> [Double]
linearRegression coefficients alpha dataset iterations 
    | iterations == 0 = coefficients
    | otherwise = 
        let thetas = newThetas coefficients alpha dataset
        in linearRegression thetas alpha dataset (iterations - 1)

-- Calculate new values for t0 and t1
-- newThetas :: Coefficients -> LerningRate -> Data -> Coefficients
newThetas :: [Double] -> Double -> [[Double]] -> [Double]
newThetas thetas alpha dataset =
    let deltas = map (calculateDelta thetas) dataset
        adjustedDeltas = adjustDeltas deltas dataset
        newt0 = t0 - alpha * avg deltas 
        newt1 = t1 - alpha * avg adjustedDeltas
    in [newt0, newt1]
    where 
        t0 = thetas !! 0
        t1 = thetas !! 1
    
-- Calculates the difference between h(x) and y
calculateDelta :: [Double] -> [Double] -> Double
calculateDelta thetas row = t0 + t1 * x - y
  where
    t0 = thetas !! 0
    t1 = thetas !! 1
    x = row !! 0
    y = row !! 1

adjustDeltas :: [Double] -> [[Double]] -> [Double]
adjustDeltas deltas dataset = 
    zipWith (*) deltas features
    where 
        features = map head dataset

avg xs = realToFrac (sum xs) / genericLength xs