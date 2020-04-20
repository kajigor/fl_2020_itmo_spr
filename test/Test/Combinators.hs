module Test.Combinators where

import           Combinators         (Parser, Result (..), Position (..), elem', runParser,
                                      runParser, satisfy, sepBy1, symbol,
                                      toStream)
import           Control.Applicative
import           Test.Helper
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

digit :: Parser String String Char
digit = satisfy (`elem` "0123456789")

unit_satisfy :: Assertion
unit_satisfy = do
    testFailure $ runParser (satisfy (/= '1')) "1234"
    testFailure $ runParser digit "blah"
    testSuccess (runParser (satisfy (== '1')) "1234") (toStream "234" (Position 1 1)) '1'
    testSuccess (runParser digit "1234") (toStream "234" (Position 1 1)) '1'

unit_elem :: Assertion
unit_elem = do
    testSuccess (runParser elem' "1234") (toStream "234" (Position 1 1)) '1'
    testSuccess (runParser elem' ([1,2,3,4] :: [Integer])) (toStream [2,3,4] (Position 1 1)) 1
    testFailure $ runParser elem' ""

unit_many :: Assertion
unit_many = do
    testSuccess (runParser (many $ symbol '1') "234") (toStream "234" (Position 1 0)) ""
    testSuccess (runParser (many $ symbol '1') "134") (toStream "34" (Position 1 1)) "1"
    testSuccess (runParser (many $ symbol '1') "114") (toStream "4" (Position 1 2)) "11"
    testSuccess (runParser (many $ symbol '1') "111") (toStream "" (Position 1 3))"111"

unit_some :: Assertion
unit_some = do
    testFailure $ runParser (some $ symbol '1') "234"
    testSuccess (runParser (some $ symbol '1') "134") (toStream "34" (Position 1 1)) "1"
    testSuccess (runParser (some $ symbol '1') "114") (toStream "4" (Position 1 2)) "11"
    testSuccess (runParser (some $ symbol '1') "111") (toStream "" (Position 1 3))"111"

unit_sepBy :: Assertion
unit_sepBy = do
    testFailure $ runParser (sepBy1 (symbol ',') digit) ""
    testSuccess (runParser (sepBy1 (symbol ',') digit) "1,4,")  (toStream "," (Position 1 3))  ['1', '4']
    testSuccess (runParser (sepBy1 (symbol ',') digit) "1,1,4") (toStream "" (Position 1 5))  ['1', '1', '4']
