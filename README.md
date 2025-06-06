# Linear Regression Implementation in Haskell

## Overview

This project implements a linear regression algorithm in Haskell using gradient descent with stochastic sampling. The implementation follows functional programming principles and includes modules for CSV parsing, evaluation metrics, and random sampling.

## Architecture

The project is organized into several modules:

- **Main.hs**: Entry point and training orchestration
- **LinearRegression.hs**: Core gradient descent implementation
- **CSV.hs**: CSV file parsing utilities
- **Evals.hs**: Model evaluation metrics
- **RandomSampling.hs**: Stochastic sampling functionality

## Module Documentation

### Main.hs

The main module composes the training process and handles user interaction.

#### Functions

##### `main :: IO ()`
Entry point that:
- Parses command line arguments (train file, test file, learning rate, iterations)
- Loads and parses training and test datasets
- Initializes model coefficients to zero
- Trains the model with progress reporting
- Evaluates final model performance

**Usage**: `<program> <train_file.csv> <test_file.csv> <learning_rate> <iterations>`

##### `trainWithProgress :: [Double] -> Double -> [[Double]] -> Int -> Int -> IO [Double]`
Recursive training function that:
- Updates model coefficients using gradient descent
- Calculates and displays MSE after each iteration
- Returns final trained coefficients

**Parameters**:
- `weights`: Current model coefficients [w0, w1]
- `alpha`: Learning rate
- `dataset`: Training data as list of [x, y] pairs
- `iterations`: Remaining iterations
- `maxIter`: Total iterations (for progress display)

##### `predict :: [Double] -> Double -> Double`
Simple linear prediction function implementing `y = w0 + w1*x`

**Parameters**:
- `weights`: Model coefficients [w0, w1]
- `x`: Input feature value

**Returns**: Predicted y value

### LinearRegression.hs

Core gradient descent implementation with stochastic sampling.

#### Functions

##### `newWeights :: [Double] -> Double -> [[Double]] -> IO [Double]`
Updates model coefficients using gradient descent with mini-batch sampling.

**Algorithm**:
1. Randomly samples 50% of the dataset
2. Calculates prediction errors (deltas)
3. Computes gradients for w0 and w1
4. Updates coefficients: `newWeight = oldWeight - alpha * gradient`

**Parameters**:
- `weights`: Current coefficients [w0, w1]
- `alpha`: Learning rate
- `dataset`: Training data

**Returns**: Updated coefficients

##### `calculateDelta :: [Double] -> [Double] -> Double`
Computes prediction error for a single data point: `h(x) - y`

##### `adjustDeltas :: [Double] -> [[Double]] -> [Double]`
Adjusts deltas by multiplying with feature values for t1 gradient calculation.

##### `avg :: [Double] -> Double`
Utility function to calculate arithmetic mean of a list.

### CSV.hs

CSV parsing utilities for loading datasets.

#### Functions

##### `parseCSV :: String -> [[Double]]`
Parses CSV content into a 2D list of doubles.

**Process**:
1. Splits content into lines
2. Splits each line by commas
3. Converts string values to Double

##### `splitByComma :: String -> [String]`
Utility function that splits a string by comma delimiters.

### Evals.hs

Model evaluation metrics.

#### Functions

##### `mse :: [Double] -> [Double] -> Double`
Calculates Mean Squared Error between predicted and actual values.

**Formula**: $MSE = \sum(predicted - actual)^2 / n$

### RandomSampling.hs

Provides stochastic sampling functionality for mini-batch gradient descent.

#### Functions

##### `sampleDataUnique :: Int -> [[Double]] -> IO [[Double]]`
Randomly samples n rows from dataset without replacement.

**Parameters**:
- `n`: Number of samples to select
- `dataset`: Source dataset

**Returns**: Randomly sampled subset



## Data Format

The implementation expects CSV files with two columns:
- Column 1: Feature values (x)
- Column 2: Target values (y)

Example:
```csv
1.0,2.5
2.0,4.1
3.0,5.8
```

## Usage Example

```bash
ghc Main.hs -o linear_regression
./linear_regression train.csv test.csv 0.01 1000
```

This command:
- Trains on `train.csv`
- Tests on `test.csv`
- Uses learning rate of 0.01
- Runs for 1000 iterations

## Output

The program provides:
1. Progress updates showing iteration number and current MSE
2. Final model coefficients (w0 and w1)
3. Training and test MSE values

## Visualization

In **notebook.ipynb** visulization of the predictions is implemented. Moreover, it is possibe to compare the predicted weights by Haskell model with the weights predicted by the built in model from skelearn library.
The notebook also includes a data prepraraion part. 

**Importatant note**: in the current version of the notebook saving of the train_data.csv and test_data.csv is commented out to avoid rewriting of the data because the CSV parser expects to have data without name of the columns so they should be deleted manually every time one rewrites the data. 

## Dependencies

- `System.Random`: For random sampling
- `System.Environment`: For command line arguments
- `System.IO`: For file operations
- `Data.List`: For list utilities

