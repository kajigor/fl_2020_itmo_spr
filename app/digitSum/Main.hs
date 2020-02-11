module Main where 

import DigitSum (evaluate)
import Text.Printf (printf)

-- "1+2"   --> 3 
-- "2+4+8" --> 14 

fm1 :: String 
fm1 = "1+2"

fm2 :: String 
fm2 = "2+4+8"

run :: String -> IO () 
run input = do 
    putStrLn ""
    putStrLn $ printf "Input:\t%s" input 
    putStrLn $ printf "Result:\t%s" (show $ evaluate input)

main :: IO () 
main = do 
    run fm1 
    run fm2 