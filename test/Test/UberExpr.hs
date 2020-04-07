module Test.UberExpr where

import           AST                 
import           Combinators         (symbol, Parser (..), Result (..), runParser, fail')
import           Control.Applicative ((<|>))
import           Expr                
import           Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import           Text.Printf         (printf)
import           UberExpr            

toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator _   = fail' "Failed toOperator"


expr1 :: Parser String String AST
expr1 =
  uberExpr [ (mult, Binary LeftAssoc)
           , (minus <|> div',Binary RightAssoc)
           , (sum',Binary NoAssoc)
           ]
           (Num <$> parseNum <|> symbol '(' *> expr1 <* symbol ')')
           BinOp UnaryOp

expr2 :: Parser String String AST
expr2 =
  uberExpr [(mult <|> div' <|> minus <|> sum',Binary LeftAssoc)]
           (Num <$> parseNum)
           BinOp UnaryOp

unit_expr1 :: Assertion
unit_expr1 = do
  runParser expr1 "13" @?= Success "" (Num 13)
  runParser expr1 "(((1)))" @?= Success "" (Num 1)
  runParser expr1 "1+2*3-4/5" @?= Success "" (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (BinOp Minus (Num 3) (BinOp Div (Num 4) (Num 5))))
  runParser expr1 "1+2+3" @?= Success "+3" (BinOp Plus (Num 1) (Num 2))
  runParser expr1 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr1 "1/2/3" @?= Success "" (BinOp Div (Num 1) (BinOp Div (Num 2) (Num 3)))
  runParser expr1 "1-2-3" @?= Success "" (BinOp Minus (Num 1) (BinOp Minus (Num 2) (Num 3)))
  runParser expr1 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Mult (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (BinOp Plus (Num 4) (Num 5)))) (BinOp Minus (Num 6) (BinOp Minus (Num 7) (BinOp Div (Num 8) (Num 9)))))

unit_expr2 :: Assertion
unit_expr2 = do
  runParser expr2 "13" @?= Success "" (Num 13)
  runParser expr2 "(((1)))" @?= Failure "Predicate failed"
  runParser expr2 "1+2*3-4/5" @?= Success "" (BinOp Div (BinOp Minus (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
  runParser expr2 "1+2+3" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1/2/3" @?= Success "" (BinOp Div (BinOp Div (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2-3" @?= Success "" (BinOp Minus (BinOp Minus (Num 1) (Num 2)) (Num 3))
  runParser expr2 "1-2*3/4+5*6-7-8/9" @?= Success "" (BinOp Div (BinOp Minus (BinOp Minus (BinOp Mult (BinOp Plus (BinOp Div (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9))

isFailure (Failure _) = True
isFailure  _          = False

unit_unaryEpxr = do
    runParser parseExpr "-1+2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1*2" @?= Success "" (BinOp Mult (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==-2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "-1&&-2" @?= Success "" (BinOp And (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!1&&!2" @?= Success "" (BinOp And (UnaryOp Not (Num 1)) (UnaryOp Not (Num 2)))
    runParser parseExpr "-1^2" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (Num 2)))
    runParser parseExpr "-1^(-2)" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (UnaryOp Minus (Num 2))))
    runParser parseExpr "(-1)^2" @?= Success "" (BinOp Pow (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1+-2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!-1" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "!(-1)" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "-(!1)" @?= Success "" (UnaryOp Minus (UnaryOp Not (Num 1)))
    runParser parseExpr "-1---2" @?= Success "---2" (UnaryOp Minus (Num 1))
   -- runParser parseExpr "-1^-2" @?= Success "^-2" (UnaryOp Minus (Num 1))

    assertBool "" $ isFailure $ runParser parseExpr "--1"
    assertBool "" $ isFailure $ runParser parseExpr "-!1"