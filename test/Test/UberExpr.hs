module Test.UberExpr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), Position (..), runParser,
                                      symbol, toStream, word)
import           Control.Applicative ((<|>))
import           Expr                (parseNum)
import           Test.Helper
import           Test.Tasty.HUnit    (Assertion (..), assertBool, (@?=))
import           Text.Printf         (printf)
import           UberExpr            (Associativity (..), OpType (..), uberExpr)

mult  = word "*" *> return Mult
sum'  = word "+" *> return Plus
minus = word "-" *> return Minus
div'  = word "/" *> return Div

expr1 :: Parser String String AST
expr1 =
  uberExpr [ (mult, Binary LeftAssoc)
           , (minus <|> div', Binary RightAssoc)
           , (sum', Binary NoAssoc)
           ]
           (Num <$> parseNum <|> symbol '(' *> expr1 <* symbol ')')
           BinOp
           UnaryOp

expr2 :: Parser String String AST
expr2 =
  uberExpr [(mult <|> div' <|> minus <|> sum', Binary LeftAssoc)]
           (Num <$> parseNum)
           BinOp
           UnaryOp

unit_expr1 :: Assertion
unit_expr1 = do
  testSuccess (runParser expr1 "13") (toStream "" $ Position 1 2) (Num 13)
  testSuccess (runParser expr1 "(((1)))") (toStream "" $ Position 1 7) (Num 1)
  testSuccess (runParser expr1 "1+2*3-4/5") (toStream "" $ Position 1 9) (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (BinOp Minus (Num 3) (BinOp Div (Num 4) (Num 5))))
  testSuccess (runParser expr1 "1+2+3") (toStream "+3" $ Position 1 3)(BinOp Plus (Num 1) (Num 2))
  testSuccess (runParser expr1 "1*2*3") (toStream "" $ Position 1 5) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  testSuccess (runParser expr1 "1/2/3") (toStream "" $ Position 1 5) (BinOp Div (Num 1) (BinOp Div (Num 2) (Num 3)))
  testSuccess (runParser expr1 "1-2-3") (toStream "" $ Position 1 5) (BinOp Minus (Num 1) (BinOp Minus (Num 2) (Num 3)))
  testSuccess (runParser expr1 "1-2*3/4+5*6-7-8/9") (toStream "" $ Position 1 17) (BinOp Mult (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (BinOp Plus (Num 4) (Num 5)))) (BinOp Minus (Num 6) (BinOp Minus (Num 7) (BinOp Div (Num 8) (Num 9)))))

unit_expr2 :: Assertion
unit_expr2 = do
  testSuccess (runParser expr2 "13") (toStream "" $ Position 1 2) (Num 13)
  assertBool "" $ isFailure $ runParser expr2 "(((1)))"
  testSuccess (runParser expr2 "1+2*3-4/5") (toStream "" $ Position 1 9) (BinOp Div (BinOp Minus (BinOp Mult (BinOp Plus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5))
  testSuccess (runParser expr2 "1+2+3") (toStream "" $ Position 1 5) (BinOp Plus (BinOp Plus (Num 1) (Num 2)) (Num 3))
  testSuccess (runParser expr2 "1*2*3") (toStream "" $ Position 1 5) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
  testSuccess (runParser expr2 "1/2/3") (toStream "" $ Position 1 5) (BinOp Div (BinOp Div (Num 1) (Num 2)) (Num 3))
  testSuccess (runParser expr2 "1-2-3") (toStream "" $ Position 1 5) (BinOp Minus (BinOp Minus (Num 1) (Num 2)) (Num 3))
  testSuccess (runParser expr2 "1-2*3/4+5*6-7-8/9") (toStream "" $ Position 1 17) (BinOp Div (BinOp Minus (BinOp Minus (BinOp Mult (BinOp Plus (BinOp Div (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (Num 3)) (Num 4)) (Num 5)) (Num 6)) (Num 7)) (Num 8)) (Num 9))
