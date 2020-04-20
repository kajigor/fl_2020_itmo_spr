module Test.Keyword where

import Combinators (Result(..), runParser, toStream)
import Control.Monad (forM)
import Keyword (keyword)
import Test.Helper
import Test.Tasty.HUnit (Assertion, (@?=))

kotlinKeywords =
  [ "as"
  , "as?"
  , "break"
  , "class"
  , "continue"
  , "do"
  , "else"
  , "false"
  , "for"
  , "fun"
  , "if"
  , "in"
  , "!in"
  , "interface"
  , "is"
  , "!is"
  , "null"
  , "object"
  , "package"
  , "return"
  , "super"
  , "this"
  , "throw"
  , "true"
  , "try"
  , "typealias"
  , "typeof"
  , "val"
  , "var"
  , "when"
  , "while"
  ]

cKeywords =
  [ "auto"
  , "break"
  , "case"
  , "char"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "extern"
  , "float"
  , "for"
  , "goto"
  , "if"
  , "int"
  , "long"
  , "register"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "struct"
  , "switch"
  , "typedef"
  , "union"
  , "unsigned"
  , "void"
  , "volatile"
  , "while"
  ]

haskellKeywords =
  [ "as"
  , "case"
  , "of"
  , "class"
  , "data"
  , "data family"
  , "data instance"
  , "default"
  , "deriving"
  , "deriving instance"
  , "do"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "then"
  , "else"
  , "import"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "in"
  , "mdo"
  , "module"
  , "newtype"
  , "proc"
  , "qualified"
  , "rec"
  , "type"
  , "type family"
  , "type instance"
  , "where"
  ]

unit_Keywords :: Assertion
unit_Keywords = do
  let suffix = "suffix"
  let prefix = "prefix"
  mapM_
    (\kw -> do
       mapM_ (\str -> testSuccess (runParser (keyword kw) str) (toStream "" (fromColumn $ length str)) str) kw
       mapM_
         (\str ->
            testSuccess
              (runParser (keyword kw) (str ++ " " ++ suffix))
              (toStream suffix (fromColumn $ length str + 1))
              str)
         kw
       mapM_
         (\str -> testSuccess (runParser (keyword kw) (str ++ "\n" ++ suffix)) (toStream suffix (fromLine 1)) str)
         kw
       mapM_ (\str -> testFailure (runParser (keyword kw) "")) kw
       mapM_ (\str -> testFailure (runParser (keyword kw) (str ++ suffix))) (filter (' ' `notElem`) kw)
       mapM_ (\str -> testFailure (runParser (keyword kw) (prefix ++ str))) kw)
    [kotlinKeywords, cKeywords, haskellKeywords]

unit_keywordsWithSpaces :: Assertion
unit_keywordsWithSpaces = do
  testSuccess (runParser (keyword ["a", "a b", "b"]) "a bc") (toStream "bc" (fromColumn 2)) "a"
  testSuccess (runParser (keyword ["a", "a b", "b"]) "a b") (toStream "" (fromColumn 3)) "a b"
  testSuccess (runParser (keyword ["a", "a b", "b"]) "a ") (toStream "" (fromColumn 2)) "a"
  
