module Polish where

import Data.Char (digitToInt, isDigit) 

-- Evaluator for arithmetic expressions in the polish notation
-- "12+"   --> 3 
-- "94-1+" --> 6
-- "123*+" --> 7 
evaluate :: String -> Int 
evaluate input = 
    go [] input 
  where 
    go stack (x:xs) | isDigit x = go (digitToInt x : stack) xs 
    go (y:x:ys) ('+':xs) = go (x + y : ys) xs 
    go (y:x:ys) ('-':xs) = go (x - y : ys) xs 
    go (y:x:ys) ('*':xs) = go (x * y : ys) xs 
    go stack [] = head stack 
    