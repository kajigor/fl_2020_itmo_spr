module Main where 

import Sum (evaluate)
import Text.Printf (printf)

-- "1+2"   --> 3 
-- "2+4+8" --> 14 
-- "11+22" --> 33
-- "13+42+777" -> 832

fm1 :: String 
fm1 = "1+2"

fm2 :: String 
fm2 = "2+4+8"

fm3 :: String 
fm3 = "11+22"

fm4 :: String 
fm4 = "13+42+777"


fm5 :: String 
fm5 = "31+24+777"

fm6 :: String 
fm6 = "12+23"

run :: String -> IO () 
run input = do 
    putStrLn ""
    putStrLn $ printf "Input:\t%s" input 
    putStrLn $ printf "Result:\t%s" (show $ evaluate input)

main :: IO () 
main = do 
    run fm1 
    run fm2 
    run fm3 
    run fm4 
    run fm5
    run fm6



