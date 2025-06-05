module Main where

import LinearRegression (newWeights)
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
                
                -- Initialize coefficients with zeros [w0, w1]
                initialWeights = [0.0, 0.0]
            
            -- Train model with progress reporting
            finalWeights <- trainWithProgress initialWeights alpha trainData iterations iterations
            
            -- Make predictions on test data
            let testFeatures = map (!! 0) testData  -- Extract x values
                testLabels = map (!! 1) testData    -- Extract y values
                predictions = map (predict finalWeights) testFeatures
                
                -- Calculate final MSEs
                trainFeatures = map (!! 0) trainData
                trainLabels = map (!! 1) trainData
                trainPredictions = map (predict finalWeights) trainFeatures
                trainMSE = mse trainPredictions trainLabels
                testMSE = mse predictions testLabels
            
            -- Print results
            putStrLn "\nFinal Model Coefficients:"
            putStrLn $ "w0 (intercept): " ++ show (finalWeights !! 0)
            putStrLn $ "w1 (slope): " ++ show (finalWeights !! 1)
            
            putStrLn "\nEvaluation Metrics:"
            putStrLn $ "Training MSE: " ++ show trainMSE
            putStrLn $ "Test MSE: " ++ show testMSE

-- Train the model and print MSE after each iteration
trainWithProgress :: [Double] -> Double -> [[Double]] -> Int -> Int -> IO [Double]
trainWithProgress weights _ _ 0  _ = return weights
trainWithProgress weights alpha dataset iterations maxIter = do
    updatedWeights <- newWeights weights alpha dataset  
    let features = map (!! 0) dataset  -- Extract x values
        labels = map (!! 1) dataset    -- Extract y values
        predictions = map (predict updatedWeights) features  
        currentMSE = mse predictions labels
    
    putStrLn $ "Iteration " ++ show (maxIter - iterations + 1) ++ 
               " of " ++ show maxIter ++ 
               ", MSE: " ++ show currentMSE
    
    trainWithProgress updatedWeights alpha dataset (iterations - 1) maxIter

-- Simple prediction function for y = w0 + w1*x
predict :: [Double] -> Double -> Double  
predict weights x = (weights !! 0) + (weights !! 1) * x