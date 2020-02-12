module Main where 

import Arith (evaluate, parseNum, parseOp, parseExpr)
import Text.Printf (printf )

fm0 :: String 
fm0 = "1"

fm1 :: String 
fm1 = "1+2"

fm2 :: String 
fm2 = "1+2*3+4"

fm3 :: String 
fm3 = "12+23*34+456"

fm4 = "2/1"
fm5 = "1*1*1*1*1/1/1/1/1/1/1*1*1*1*1*1*1*1*1/1"



run :: String -> IO () 
run input = do 
    putStrLn ""
    putStrLn $ printf "Input:\t%s" input 
    case parseExpr input of 
      Just (tree, rest) -> 
        putStrLn $ printf "Rest:\t%s\nResult:\t%s\nTree:%s\n" (show rest) (show $ evaluate input) (show tree) 
      Nothing -> putStrLn "Parsing failed"

main :: IO () 
main = do 
    run fm0
    run fm1 
    run fm2
    run fm3
    run fm4
    run fm5

