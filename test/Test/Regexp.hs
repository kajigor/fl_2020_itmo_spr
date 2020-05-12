module Test.Regexp where

import Test.HUnit (Assertion, assertBool)
import Regexp (Regexp (..), match)


a :: Regexp
a = Char 'a'

b :: Regexp
b = Char 'b'

c :: Regexp
c = Char 'c'

-- a*a
r1 :: Regexp
r1 = Seq (Star a) a

-- (a|a)*
r2 :: Regexp
r2 = Star (Alt a a)

-- b | (c (a|b)*)
r3 :: Regexp
r3 = Alt b (Seq c (Star (Alt a b)))

unit_regexp :: Assertion
unit_regexp = do
  assertBool "a*a" (match r1 (replicate 50 'a'))
  assertBool "a*a" (not $ match r1 (replicate 50 'a' ++ "b"))

  assertBool "(a|a)*" (match r2 (replicate 50 'a'))
  assertBool "(a|a)*" (not $ match r2 (replicate 50 'a' ++ "b"))

  assertBool "b|c(a|b)*" (match r3 "b")
  assertBool "b|c(a|b)*" (match r3 "c")
  assertBool "b|c(a|b)*" (match r3 "cabba")
  assertBool "b|c(a|b)*" (not $ match r3 "d")
  assertBool "b|c(a|b)*" (not $ match r3 "ba")
  assertBool "b|c(a|b)*" (not $ match r3 "aaaa")