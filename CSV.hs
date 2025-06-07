module CSV (parseCSV, splitByComma) where

-- Parse a CSV string into a list of lists 
parseCSV :: String -> [[Double]]
parseCSV = map (map read . splitByComma) . lines

-- Split a string by commas
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (',' : xs) = "" : rest
  where rest = splitByComma xs
splitByComma (x : xs) = (x : head rest) : tail rest
  where rest = splitByComma xs
