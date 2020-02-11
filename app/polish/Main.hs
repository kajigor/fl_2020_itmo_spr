module Main where 

import Polish (evaluate)
import Text.Printf (printf)

fm1 :: String 
fm1 = "12+"

fm2 :: String 
fm2 = "94-1+"

fm3 :: String 
fm3 = "123*+"

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