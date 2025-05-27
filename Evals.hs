module Evals(mse) where

-- Compute the MSE of two vectors
mse :: [Double] -> [Double] -> Double
mse x y = sum (zipWith (\a b -> (a - b) * (a - b)) x y) / fromIntegral (length x)