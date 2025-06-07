# Linear Regression Implementation in Haskell

## Overview

This project implements a multiple linear regression algorithm in Haskell using gradient descent with stochastic sampling. 

## Usage Example

```bash
ghc Main -o linear_regression
./linear_regression train_data.csv test_data.csv 0.01 1000
```

This command:
- Trains on `train.csv`
- Tests on `test.csv`
- Uses learning rate of 0.01
- Runs for 1000 iterations

## Module Documentation

### == Main.hs ==

Composes the training process and handles user interaction.

`main :: IO ()`  
Parses input of the user and prints the results of the training.

`trainWithProgress :: [Double] weights -> Double alpha-> [[Double]] dataset -> Int iterations -> Int maxIterations -> IO [Double]`  
Returns the weights of the linear regression after specified number of iterations on the given dataset.

`predict :: [Double] -> Double -> Double`  
Returns the predicted value for the row of features.


### == LinearRegression.hs ==

Includes implementation of the methods required for training of the linear regression.

`newWeights :: [Double] -> Double -> [[Double]] -> IO [Double]`   
Returns updated values of weights after one SGD iteration.

`calculateDelta :: [Double] -> [Double] -> Double`   
Return the difference between the predicted value of the target and actual one.

`avg :: [Double] -> Double`  
Returns the average value for the list.

### == CSV.hs ==

Includes CSV parsing utilities for loading datasets.

`parseCSV :: String -> [[Double]]`  
Returns parsed CSV file as a list of lists. 

`splitByComma :: String -> [String]`   
Returns a list of strings obtained by splitting a string by commas.

### == Evals.hs ==

Model evaluation metrics.

`mse :: [Double] -> [Double] -> Double`   
Returns MSE(mean squred error) value for the given predictions.


### == RandomSampling.hs ==

Provides stochastic sampling functionality for mini-batch gradient descent.

`sampleDataUnique :: Int -> [[Double]] -> IO [[Double]]`   
Returns some specified number of randomly picked lists from the lists of lists. 

## Reference to the dataset

The datasets for training and testing of the model were derived from the following dataset — https://www.kaggle.com/datasets/yasserh/student-marks-dataset
