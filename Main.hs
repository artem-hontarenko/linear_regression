module Main where

import LinearRegression (newThetas)
import Data.List (intercalate)
import System.Environment (getArgs)
import CSV
import Evals
import System.IO (readFile)

main :: IO ()
main = do
    args <- getArgs
    if length args < 4
        then putStrLn "Usage: <program> <train_file.csv> <test_file.csv> <learning_rate> <iterations>"
        else do
            let trainFile = args !! 0
                testFile = args !! 1
                alpha = read (args !! 2) :: Double
                iterations = read (args !! 3) :: Int
            
            trainContent <- readFile trainFile
            testContent <- readFile testFile
            
            let trainData = parseCSV trainContent
                testData = parseCSV testContent
                
                -- Initialize coefficients with zeros [t0, t1]
                initialThetas = [0.0, 0.0]
            
            -- Train model with progress reporting
            finalThetas <- trainWithProgress initialThetas alpha trainData iterations
            
            -- Make predictions on test data
            let testFeatures = map (!! 0) testData  -- Extract x values
                testLabels = map (!! 1) testData    -- Extract y values
                predictions = map (predict finalThetas) testFeatures
                
                -- Calculate final MSEs
                trainFeatures = map (!! 0) trainData
                trainLabels = map (!! 1) trainData
                trainPredictions = map (predict finalThetas) trainFeatures
                trainMSE = mse trainPredictions trainLabels
                testMSE = mse predictions testLabels
            
            -- Print results
            putStrLn "\nFinal Model Coefficients:"
            putStrLn $ "t0 (intercept): " ++ show (finalThetas !! 0)
            putStrLn $ "t1 (slope): " ++ show (finalThetas !! 1)
            
            putStrLn "\nPredictions on Test Dataset:"
            putStrLn $ intercalate "\n" $ map show predictions
            
            putStrLn "\nEvaluation Metrics:"
            putStrLn $ "Training MSE: " ++ show trainMSE
            putStrLn $ "Test MSE: " ++ show testMSE

-- Train the model and print MSE after each iteration
trainWithProgress :: [Double] -> Double -> [[Double]] -> Int -> IO [Double]
trainWithProgress thetas _ _ 0 = return thetas
trainWithProgress thetas alpha dataset iterations = do
    let updatedThetas = newThetas thetas alpha dataset
        features = map (!! 0) dataset  -- Extract x values
        labels = map (!! 1) dataset    -- Extract y values
        predictions = map (predict updatedThetas) features
        currentMSE = mse predictions labels
    
    putStrLn $ "Iteration " ++ show (maxIter - iterations + 1) ++ 
               " of " ++ show maxIter ++ 
               ", MSE: " ++ show currentMSE
    
    trainWithProgress updatedThetas alpha dataset (iterations - 1)
    where maxIter = iterations

-- Simple prediction function for y = t0 + t1*x
predict :: [Double] -> Double -> Double
predict thetas x = (thetas !! 0) + (thetas !! 1) * x