module LLang where

import Control.Applicative (Alternative (..))
import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), Result(..), satisfy, many', symbol)
import Expr hiding (parseExpr)
import UberExpr
import Data.Char
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)


data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Eq)

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

-- я не писала парсер для keywords в HW04, поэтому сделала что-то простое
-------------------------------------------------------------------------

compareStr :: String -> String -> (Bool, String)
compareStr (x:xs) (y:ys) = if (x == y) then (compareStr xs ys)
                        else (False, "") 
compareStr xs [] = (True, xs)
compareStr _ _  = (False, "") 


satisfyStr :: String  -> Parser String String String
satisfyStr p =  Parser $ \input -> 
  if ((length p) > (length input)) then Failure $ "Error key word" 
  else if (fst $ compareStr input p) then Success (snd $ compareStr input p) p 
       else Failure $ "Error key* word"
     
simpleParseKeyword s = parseSeparators *> (satisfyStr s) <* parseSeparators

--------------------------------------------------------------------------

-- добавила в Ast.hs T и F 
toAST :: String -> Parser String String AST
toAST "true" = return $ T
toAST "false" = return $ F

-- старый парсер для выражений без унарного минуса
parseExpr :: Parser String String AST
parseExpr =  uberExpr [(parseOrOp, Binary RightAssoc),
                     (parseAndOp, Binary RightAssoc),
                     (parseNotOp, Unary),
                     (parseGeqOp <|> parseLeqOp <|> parseLtOp <|> parseGtOp <|> parseEqOp <|> parseNeqOp, Binary NoAssoc),
                     (parseAddOp <|> parseSubOp, Binary LeftAssoc),
                     (parseMultOp <|> parseDivOp, Binary LeftAssoc),
                     (parsePowOp, Binary RightAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')' <|> Ident <$> parseIdent)
           BinOp
           UnaryOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator
               parsePowOp = symbol '^' >>= toOperator
               parseLtOp = symbol '<' >>= toOperator
               parseGtOp = symbol '>' >>= toOperator
               parseNotOp = symbol '!' >>= toOperator
               parseOrOp = (:) <$> (symbol '|') <*> ((:[]) <$> (symbol '|')) >>= toOperatorStr
               parseAndOp = (:) <$> (symbol '&') <*> ((:[]) <$> (symbol '&')) >>= toOperatorStr
               parseLeqOp = (:) <$> (symbol '<') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseGeqOp = (:) <$> (symbol '>') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseEqOp = (:) <$> (symbol '=') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseNeqOp = (:) <$> (symbol '/') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr


--парсер для выражений вида: Expr LogikOp Expr
parseLogikExpr :: Parser String String AST
parseLogikExpr =  uberExpr [(parseOrOp, Binary RightAssoc),
                     (parseAndOp, Binary RightAssoc),
                     (parseGeqOp <|> parseLeqOp <|> parseLtOp <|> parseGtOp <|> parseEqOp <|> parseNeqOp, Binary NoAssoc)
                     ]
           (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')' <|> Ident <$> parseIdent)
           BinOp
           UnaryOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator
               parsePowOp = symbol '^' >>= toOperator
               parseLtOp = symbol '<' >>= toOperator
               parseGtOp = symbol '>' >>= toOperator
               parseNotOp = symbol '!' >>= toOperator
               parseOrOp = (:) <$> (symbol '|') <*> ((:[]) <$> (symbol '|')) >>= toOperatorStr
               parseAndOp = (:) <$> (symbol '&') <*> ((:[]) <$> (symbol '&')) >>= toOperatorStr
               parseLeqOp = (:) <$> (symbol '<') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseGeqOp = (:) <$> (symbol '>') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseEqOp = (:) <$> (symbol '=') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseNeqOp = (:) <$> (symbol '/') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr


--возможно выражение: !(Expr LogikOp Expr) -- без пробелов между cкобками и '!'
parseCondition = parseSeparators *> parseInside <* parseSeparators
               where
                   parseTrue = (simpleParseKeyword "true") >>= toAST
                   parseFalse = (simpleParseKeyword "false") >>= toAST
                   parseNotLogikExpr = do
                      not <- symbol '!'
                      left <- symbol '('
                      result <- parseLogikExpr
                      right <- symbol ')'
                      return $ UnaryOp Not result
                   parseInside = parseTrue <|> parseFalse <|> parseLogikExpr <|> parseNotLogikExpr
 
 
                  
parseAssign = Assign <$> parseId <*> (symbol '=' *> parseExp)
            where 
               parseId = parseSeparators *> parseIdent <* parseSeparators
               parseExp = parseSeparators *> parseExpr <* parseSeparators

parseRead = Read <$> (simpleParseKeyword "read" *> parseR) where
                    parseR = do 
                      sep1 <- parseSeparators
                      left <- symbol '('
                      sep2 <- parseSeparators
                      result <- parseIdent
                      sep3 <- parseSeparators
                      right <- symbol ')'
                      sep4 <- parseSeparators
                      return $ result

parseWrite = Write <$> (simpleParseKeyword "write" *> parseW) where
                parseW = do 
                      sep1 <- parseSeparators
                      left <- symbol '('
                      sep2 <- parseSeparators
                      result <- parseExpr
                      sep3 <- parseSeparators
                      right <- symbol ')'
                      sep4 <- parseSeparators
                      return $ result

parseWhile = While <$> (simpleParseKeyword "while" *> parseC) <*> parseS where
                        parseC = do 
                          sep1 <- parseSeparators
                          left <- symbol '('
                          sep2 <- parseSeparators
                          result <- parseCondition
                          sep3 <- parseSeparators
                          right <- symbol ')'
                          sep4 <- parseSeparators
                          return $ result
                        parseS = do 
                          sep1 <- parseSeparators
                          left <- symbol '{'
                          sep2 <- parseSeparators
                          result <- parseSeq
                          sep3 <- parseSeparators
                          right <- symbol '}'
                          sep4 <- parseSeparators
                          return $ result

parseIf = If <$> (simpleParseKeyword "if" *> parseC) <*> parseS <*> (simpleParseKeyword "else" *> parseS ) where
                        parseC = do 
                          sep1 <- parseSeparators
                          left <- symbol '('
                          sep2 <- parseSeparators
                          result <- parseCondition
                          sep3 <- parseSeparators
                          right <- symbol ')'
                          sep4 <- parseSeparators
                          return $ result
                        parseS = do 
                          sep1 <- parseSeparators
                          left <- symbol '{'
                          sep2 <- parseSeparators
                          result <- parseSeq
                          sep3 <- parseSeparators
                          right <- symbol '}'
                          sep4 <- parseSeparators
                          return $ result

parseEnd = simpleParseKeyword ";"

parseStatment = (parseSt parseIf) <|> (parseSt parseWhile) <|> (parseSt parseWrite) <|> (parseSt parseRead) <|> (parseSt parseAssign)
               where
                  parseSt parser = do 
                     sep1 <- parseSeparators
                     result <- parser
                     sep2 <- parseSeparators
                     ens <- parseEnd
                     sep3 <- parseSeparators
                     return $ result

parseStatments =  many' parseStatment

parseSeparator :: Parser String String Char
parseSeparator = (satisfy isSeparator) <|> (symbol '\n')

parseSeparators = many' parseSeparator

parseSeq :: Parser String String LAst
parseSeq = Seq <$> (parseSeparators *> parseStatments <* parseSeparators)

parseL :: Parser String String LAst
parseL = parseSeq


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []


eval :: LAst -> Configuration -> Maybe Configuration
eval (Assign var expr) (Conf dict input output) = do 
          result <- evalExpr dict expr
          let new_dict = Map.insert var result dict
          return $ Conf new_dict input output
eval (Read var) (Conf dict input output) = do
          case input of
             [] -> Nothing
             (x:xs) -> let new_dict = Map.insert var (head input) dict in 
                       return $ Conf new_dict input output
eval (Write expr) (Conf dict input output) = do
           result <- evalExpr dict expr
           return $ Conf dict input (result:output)
eval (While cond body) (Conf dict input output) = do
           result <- evalExpr dict cond
           if (intToBool result) then do
              new_conf <- eval body (Conf dict input output)
              result' <- eval (While cond body) new_conf
              return $ result'
           else return $ (Conf dict input output)
eval (If cond thn els) (Conf dict input output) = do
      result <- evalExpr dict cond
      if (intToBool result) then do
         result' <-  eval thn (Conf dict input output)
         return $ result'
      else do
         result' <-  eval els (Conf dict input output)
         return $ result'
eval (Seq []) (Conf dict input output) = return $ (Conf dict input output)
eval (Seq xs) (Conf dict input output) = do
        (Conf dict' input' output') <- eval (head xs) (Conf dict input output)
        eval (Seq (tail xs)) (Conf dict' input' output')


instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
