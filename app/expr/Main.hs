module Main where

import           Combinators (runParser, Result (..))
import           Expr        (evaluate, parseExpr, parseNum, parseOp)
import           Text.Printf (printf)

fm0 :: String
fm0 = "1"

fm1 :: String
fm1 = "1+2"

fm2 :: String
fm2 = "1+2*3+4"

fm3 :: String
fm3 = "12+23*34+456"

run :: String -> IO ()
run input = do
    putStrLn ""
    putStrLn $ printf "Input:\t%s" input
    case runParser parseExpr input of
      Success rest tree ->
        putStrLn $ printf "Rest:\t%s\nResult:\t%s\nTree:%s\n" (show rest) (show $ evaluate input) (show tree)
      Failure e -> putStrLn $ "Parsing failed:\n" ++ show e

main :: IO ()
main = do
    run fm0
    run fm1
    run fm2
    run fm3
