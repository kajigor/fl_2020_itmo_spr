module Test.Keyword where

import           Combinators      (Result (..), runParser)
import           Control.Monad    (forM)
import           Keyword          (keyword)
import           Test.Tasty.HUnit (Assertion, (@?=))

kotlinKeywords = ["as", "as?", "break", "class", "continue", "do", "else", "false", "for", "fun", "if", "in", "!in", "interface", "is", "!is", "null", "object", "package", "return", "super", "this", "throw", "true", "try", "typealias", "typeof", "val", "var", "when", "while"]

cKeywords = ["auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"]

haskellKeywords = ["as", "case", "of", "class", "data", "data family", "data instance", "default", "deriving", "deriving instance", "do", "forall", "foreign", "hiding", "if", "then", "else", "import", "infix", "infixl", "infixr", "instance", "let", "in", "mdo", "module", "newtype", "proc", "qualified", "rec", "type", "type family", "type instance", "where" ]

isFailure (Failure _) = True
isFailure  _          = False

unit_Keywords :: Assertion
unit_Keywords = do
  let suffix = "suffix"
  let prefix = "prefix"

  mapM_
    (\kw -> do
      mapM_ (\str -> runParser (keyword kw)  str @?= Success "" str) kw
      mapM_ (\str -> runParser (keyword kw)  (str ++ " " ++ suffix) @?= Success suffix str) kw
      mapM_ (\str -> runParser (keyword kw)  (str ++ "\n" ++ suffix) @?= Success suffix str) kw
      mapM_ (\str -> isFailure (runParser (keyword kw) "") @?= True) kw
      mapM_ (\str -> isFailure (runParser (keyword kw) (str ++ suffix)) @?= True) (filter (' ' `notElem`) kw)
      mapM_ (\str -> isFailure (runParser (keyword kw) (prefix ++ str)) @?= True) kw
    )
    [kotlinKeywords, cKeywords, haskellKeywords]

unit_keywordsWithSpaces :: Assertion
unit_keywordsWithSpaces = do
  runParser (keyword ["a", "a b", "b"]) "a bc" @?= Success "bc" "a"
  runParser (keyword ["a", "a b", "b"]) "a b" @?= Success "" "a b"
  runParser (keyword ["a", "a b", "b"]) "a " @?= Success "" "a"