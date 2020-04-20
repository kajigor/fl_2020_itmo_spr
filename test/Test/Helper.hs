module Test.Helper where

import           Combinators      (ErrorMsg (..), InputStream (..), Parser,
                                   Position (..), Result (..), initPosition)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

testFailure = assertBool "" . isFailure

testSuccess (Success s _ r) str ast = (s, r) @?= (str, ast)

isFailure (Failure _) = True
isFailure _           = False

erasePosition :: Result e i a -> Result e i a
erasePosition (Success str _ x) = Success (InputStream (stream str) initPosition) Nothing x
erasePosition x = x

fromColumn = Position 0

fromLine line = Position line 0

getPosition (Success (InputStream _ pos) _ _) = return pos
getPosition (Failure (Just (ErrorMsg _ pos))) = return pos
getPosition (Failure Nothing)                 = Nothing
