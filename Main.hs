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
            let trainFile = head args
                testFile = args !! 1
                alpha = read (args !! 2) :: Double
                iterations = read (args !! 3) :: Int
            
            trainContent <- readFile trainFile
            testContent <- readFile testFile
            
            let trainData = parseCSV trainContent
                testData = parseCSV testContent
                
                -- Determine number of features 
                numFeatures = length (head trainData) - 1
                
                -- Initialize weights
                initialWeights = replicate (numFeatures + 1) 0.0
            
            putStrLn $ "Training model with " ++ show numFeatures ++ " features..."
            
            -- Train model with progress reporting
            finalWeights <- trainWithProgress initialWeights alpha trainData iterations iterations
            
            -- Make predictions on test data
            let testFeatures = map init testData     
                testLabels = map last testData       
                predictions = map (predict finalWeights) testFeatures
                
                -- Calculate final MSEs
                trainFeatures = map init trainData
                trainLabels = map last trainData
                trainPredictions = map (predict finalWeights) trainFeatures
                trainMSE = mse trainPredictions trainLabels
                testMSE = mse predictions testLabels
            
            -- Print results
            putStrLn "\nFinal Model Coefficients:"
            putStrLn $ "w0 (intercept): " ++ show (head finalWeights)
            mapM_ (\(i, coef) -> putStrLn $ "w" ++ show i ++ ": " ++ show coef) 
                  (zip [1..] (tail finalWeights))
            
            putStrLn "\nEvaluation Metrics:"
            putStrLn $ "Training MSE: " ++ show trainMSE
            putStrLn $ "Test MSE: " ++ show testMSE

-- Train the model and print MSE after each iteration
trainWithProgress :: [Double] -> Double -> [[Double]] -> Int -> Int -> IO [Double]
trainWithProgress weights _ _ 0  _ = return weights
trainWithProgress weights alpha dataset iterations maxIter = do
    updatedWeights <- newWeights weights alpha dataset  
    
    let features = map init dataset     
        labels = map last dataset       
        predictions = map (predict updatedWeights) features  
        currentMSE = mse predictions labels
    
    putStrLn $ "Iteration " ++ show (maxIter - iterations + 1) ++ 
               " of " ++ show maxIter ++ 
               ", MSE: " ++ show currentMSE
    
    trainWithProgress updatedWeights alpha dataset (iterations - 1) maxIter

-- Multiple features prediction function for y = w0 + w1*x1 + w2*x2 + ... + wn*xn
predict :: [Double] -> [Double] -> Double
predict weights features = 
    head weights + sum (zipWith (*) (tail weights) features)
