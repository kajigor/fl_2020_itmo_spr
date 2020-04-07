module Test.LLangParser where

import           AST                 
import           Combinators         (symbol, Parser (..), Result (..), runParser, fail')
import           Control.Applicative ((<|>))
import           Expr                
import           Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import           Text.Printf         (printf)
import           UberExpr            
import           LLangParser

isFailure (Failure _) = True
isFailure  _          = False

unit_expr1 :: Assertion
unit_expr1 = do
  isFailure (runParser parseLLang "a=3;") @?= False
  isFailure (runParser parseLLang "a=3") @?= True
  isFailure (runParser parseLLang "read(a);") @?= False
  isFailure (runParser parseLLang "write(3);") @?= False
  isFailure (runParser parseLLang "read(3);") @?= True
  isFailure (runParser parseLLang "while(a==2){a=2;};") @?= False
  isFailure (runParser parseLLang "if(a==2){a=2;b=3;c=5;read(a);}else{a=2;};") @?= False

