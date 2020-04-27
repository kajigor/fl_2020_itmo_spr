module Test.LLangParser where

import           AST                 
import           Combinators         
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

unit_expr2 = do
  isFailure (runParser parseProg "def main(){a=3;write(a);} def f(){a=3;return a;}") @?= False
  isFailure (runParser parseProg "def main1(){a=3;write(a);} def f(){a=3;return a;}") @?= True
  isFailure (runParser parseProg "def main(){a=3;write(f(2,f(3),a));} def f(a, b, c){a=3;return a;}") @?= False
  isFailure (runParser parseProg "def main(){return f(2,3,4);}") @?= False


error_expr = do
  let (Success i _ _) =  runParser parseLLang "a=3;\nb=3;eqweqw"
  i @?= (InputStream "eqweqw" (Position 1 4))
  let (Success i1 _ _) =  runParser parseLLang "a=3;\tb=3;eqweqw"
  i1 @?= (InputStream "eqweqw" (Position 0 16))  

