module Test.Helper where

import           Combinators      (Parser, Result (..), InputStream (..), Position(..), initPosition, toStreamInitPos)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

testFailure = assertBool "" . isFailure

testSuccess (Success s _ r) str ast = (s, r) @?= (str, ast)

isFailure (Failure _) = True
isFailure _           = False

erasePosition :: Result e i a -> Result e i a
erasePosition (Success str _ x) = Success (InputStream (stream str) initPosition) Nothing x
erasePosition x                 = x

testSuccessErase result str ast = testSuccess (erasePosition result) (toStreamInitPos str) ast

