module CSV (parseCSV, splitByComma) where

parseCSV :: String -> [[Double]]
parseCSV = map (map read . splitByComma) . lines

splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (',' : xs) = "" : rest
  where rest = splitByComma xs
splitByComma (x : xs) = (x : head rest) : tail rest
  where rest = splitByComma xs