module Sum where 

import Data.Char (digitToInt)

-- Evaluator for summation 
-- "1+2"   --> 3 
-- "2+4+8" --> 14 
-- "11+22" --> 13
evaluate :: String -> Int 
evaluate = 
    sum . map parseNum . splitOn '+' 

parseNum :: String -> Int 
parseNum = 
    foldl (\acc d -> 10 * acc + digitToInt d) 0

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = 
    go xs [] 
  where
    go (y:ys) acc | x == y = reverse acc : go ys [] 
                  | otherwise = go ys (y:acc)
    go [] acc = [reverse acc] 