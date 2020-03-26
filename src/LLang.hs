module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), symbol)
import Expr
import Control.Applicative(Alternative (..))
import Keyword

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

spaceChars = (symbol ' ' <|> symbol '\t' <|> symbol '\n')
manySpace = many spaceChars
someSpace = some spaceChars

assignOpParser = manySpace *> symbol '=' <* manySpace
semiOpParser = manySpace *> symbol ';' <* manySpace

-- Показалось, что так будет удобнее, чем через keywords.
-- Все равно нам нужно не просто считать ключевое слово,
-- но и убедиться, что считалось какое-то конкретное
parseString :: String -> Parser String String String
parseString [] = pure ""
parseString (x:xs) = (:) <$> symbol x <*> parseString xs

parseBrackets (op,cl) p = (manySpace *> symbol op <* manySpace) 
                          *> p <*
                          (manySpace *> symbol cl <* manySpace)
parseRound = parseBrackets ('(', ')')
parseCurly = parseBrackets ('{', '}')

parseL :: Parser String String LAst
parseL = lFromList <$> some parseStatement where
  lFromList :: [LAst] -> LAst
  lFromList [x] = x
  lFromList xs = Seq xs
  -- To allow for empty blocks in while and if
  parseL' = parseL <|> (const (Seq []) <$> manySpace)
  parseStatement :: Parser String String LAst
  parseStatement = parseAssign <|> parseCond <|> parseWhile <|> parseWrite <|> parseRead
  parseAssign = manySpace *> (Assign <$> parseIdent <* assignOpParser <*> parseExpr) <* semiOpParser
  parseCond = manySpace *> (If <$> (parseString "if" *> parseRound parseExpr) <*> 
                                   (parseString "then" *> parseCurly parseL')  <*>
                                   (parseString "else" *> parseCurly parseL'))
  parseWhile = manySpace *> (While <$> (parseString "while" *> parseRound parseExpr) <*>
                                       parseCurly parseL')
  parseWrite = manySpace *> (Write <$> (parseString "write" *> parseRound parseExpr)) <* semiOpParser
  parseRead = manySpace *> (Read <$> (parseString "read" *> someSpace *> parseIdent)) <* semiOpParser
