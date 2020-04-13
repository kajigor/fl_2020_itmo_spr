module Test.LLangEvaluator where

import           AST                 
import           Combinators         (symbol, Parser (..), Result (..), runParser, fail')
import           Control.Applicative ((<|>))
import           Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import           Text.Printf         (printf)
import           LLangEvaluator

isFailure (Failure _) = True
isFailure  _          = False

unit_expr1' :: Assertion
unit_expr1' = do
  evaluate' "if(2==2){a=2;b=3;c=5;}else{a=4;b=5;};write(a);write(b);" [] @?= Just [3,2]
  evaluate' (unlines ["n = 5;",
          "a = 0;",
          "b = 1;",
          "i = 1;",
          "while(i<n){",
          "c=a+b;",
          "a=b;",
          "b=c;",
          "i=i+1;",
          "};",
          "write(b);"]) [] @?= Just [5]
  evaluate' "read(x);write(x*2);" [2] @?= Just [4]
  evaluate' "read(n);i=0;while(i<n){write(i);i=i+1;};" [2] @?= Just [1, 0]




