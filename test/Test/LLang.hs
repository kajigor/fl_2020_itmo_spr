module Test.LLang where

import           Combinators      (Result (..), runParser, symbols)
import           LLang           hiding ()
import           Test.Tasty.HUnit (Assertion, (@?=))
import           AST              (AST (..), Operator (..))

success :: a -> Result String String a
success = Success ""


unit_InBrackets :: Assertion
unit_InBrackets = do
    runParser (inBrackets (symbols "hello")) "(hello)" @?= Success "" "hello"
    runParser (inBrackets (symbols "hello")) "(hello" @?= Failure "Predicate failed"
    runParser (inBrackets (symbols "hello")) "((hello))" @?= Failure "Predicate failed"
    runParser (inBrackets (symbols "(hello)")) "(hello)" @?= Failure "Predicate failed"
    runParser (inBrackets (symbols "(hello)")) "((hello))" @?= Success "" "(hello)"

unit_parseCondition :: Assertion
unit_parseCondition = do
    runParser parseCondition "(1+1)" @?= Success "" (BinOp Plus (Num 1) (Num 1))
    runParser parseCondition "((1+1))" @?= Success "" (BinOp Plus (Num 1) (Num 1))
    runParser parseCondition "((((((1+1))))))" @?= Success "" (BinOp Plus (Num 1) (Num 1))

unit_parseVarName :: Assertion
unit_parseVarName = do
    runParser parseVarName "if" @?= Failure ""
    runParser parseVarName "while" @?= Failure ""
    runParser parseVarName "hello" @?= success "hello"
    runParser parseVarName "if'" @?= success "if'"

unit_parseArgs :: Assertion
unit_parseArgs = do
    runParser parseArgs "(1+1,3,7,-12)" @?= success [(BinOp Plus (Num 1) (Num 1)), (Num 3), (Num 7), (UnaryOp Minus (Num 12))]
    runParser parseParams "()" @?= success []

unit_parseParams :: Assertion
unit_parseParams = do
    runParser parseParams "(a,b,c,d,e,f,g)" @?= success ["a","b","c","d","e","f","g"]
    runParser parseParams "(a,b,c,d,e,f,g) do something else" @?= Success " do something else" ["a","b","c","d","e","f","g"]
    runParser parseParams "(if)" @?= Failure "Predicate failed"
    runParser parseParams "()" @?= success []

unit_parseDoDOne :: Assertion
unit_parseDoDOne = do
    let dd = runParser parseDoDone
    dd "do done" @?= success (Seq [])
    dd "do let a=10done" @?= success (Seq [Let "a" (Num 10)])
    dd "do let a=(1+1);let a'=1;function write(a);function read(a)done" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1))), Let "a'" (Num 1), FunctionCall "write" [Ident "a"], FunctionCall "read" [Ident "a"]])
 

unit_parseStatements :: Assertion
unit_parseStatements = do
    runParser parseStatements "let a=(1+1)" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1)))])
    runParser parseStatements "let a=(1+1);let a'=1" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1))), Let "a'" (Num 1)])
    runParser parseStatements "let a=(1+1);let a'=a" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1))), Let "a'" (Ident "a")])
    runParser parseStatements "let a=(1+1);let a'=1;function write(a)" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1))), Let "a'" (Num 1), FunctionCall "write" [Ident "a"]])
    runParser parseStatements "let a=(1+1);let a'=1;function write(a,17+15);function read(a')" @?= success (Seq [(Let "a" (BinOp Plus (Num 1) (Num 1))), Let "a'" (Num 1), FunctionCall "write" [Ident "a", BinOp Plus (Num 17) (Num 15)], FunctionCall "read" [Ident "a'"]])

    runParser parseStatements "while (1)do done" @?= success (Seq [(While (Num 1) (Seq []))])
    runParser parseStatements "let x=10;while (!(x<10))do let x=x-10done" @?= success (Seq [Let "x" (Num 10), (While (UnaryOp Not (BinOp Lt (Ident "x") (Num 10))) (Seq [Let "x" (BinOp Minus (Ident "x") (Num 10))]))])

