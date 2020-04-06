module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), alt, many', symbol, symbols, map', fail')
import Expr (parseExpr, parseIdent)
import Keyword (keyword)
import Control.Monad (guard)

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Let { var :: Var, expr :: Expr }
  | Function { funcname :: Var, params :: [Var], funcbody :: LAst }
  | FunctionCall { funcname :: Var, args :: [Expr] }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

keywords :: [String]
keywords = ["if", "else", "elif", "do", "done", "while", "let", "function"]
statementSeparator = ';'
argsSeparator = ','
paramsSeparator = ','

-- statements ::= statement ';' | statement ';' statements
--
-- statement ::= 'if' condition 'then' dodone { 'elif' condition dodone } ['else' dodone]
--             | 'while' condition dodone
--             | 'let' VARNAME '=' expression
--             | 'let' 'function' VARNAME params dodone    -- function definition
--             | 'function' args                           -- function call 
--
-- condition ::= '(' expression ')'
-- dodone ::= 'do' statements 'done'
-- params ::= '(' VARNAME { ',' VARNAME } ')'
-- args ::= '(' expression { ',' expression } ')'

parseWhitespace :: Parser String String String
parseWhitespace = many' (symbol ' ' `alt` symbol '\n' `alt` symbol '\t')

ignoreWhitespace :: Parser String String a -> Parser String String a
ignoreWhitespace parser = parseWhitespace *> parser <* parseWhitespace

inBrackets :: Parser String String a -> Parser String String a
inBrackets parser = symbol '(' *> parser <* symbol ')' 

parseCondition :: Parser String String AST
parseCondition = inBrackets parseExpr

parseRepeats :: Parser String String a -> Char -> Parser String String [a]
parseRepeats parser sep = parser' `alt` parser'' `alt` return []
    where parser' = do
            var <- parser
            rest <- parseRepeats parser sep
            return $ var:rest
          parser'' = do
            symbol sep
            var <- parser
            rest <- parseRepeats parser sep
            return $ var:rest

parseParams = inBrackets $ parseRepeats parseVarName paramsSeparator
parseArgs = inBrackets $ parseRepeats parseExpr argsSeparator

parseDoDone :: Parser String String LAst
parseDoDone = do
    checkKeyword "do"
    statements <- parseStatements
    checkKeyword "done"
    return statements

parseVarName :: Parser String String String
parseVarName = do
    name <- parseIdent
    guard (name `notElem` keywords)
    return name

-- Проверяет, что переданная строка является ключевым словом и находится в начале input'а. 
checkKeyword :: String -> Parser String String String
checkKeyword key = do
    word <- keyword keywords
    guard (key == word)
    return word

parseStatement :: String -> Parser String String LAst
parseStatement "if" = do
        condition <- parseCondition
        checkKeyword "then"
        thenBlock <- parseDoDone
        elseBlock <- elseBlockParser
        return $ If condition thenBlock elseBlock
    where elseBlockParser =  parseElif `alt` parseElse `alt` return (Seq [])
          parseElif = do
                symbols "el"
                if' <- checkKeyword "if" 
                parseStatement if'
          parseElse = do
                checkKeyword "else"
                parseDoDone
                
parseStatement "while" = do
    condition <- parseCondition
    statements <- parseDoDone 
    return $ While condition statements

parseStatement "let" =  parseVarAssign `alt` parseFuncAssign
    where parseVarAssign = do
                name <- parseVarName
                symbol '='
                expr <- parseExpr
                return $ Let name expr 
          parseFuncAssign = do
                func <- keyword keywords
                guard (func == "function")
                name <- parseVarName
                params <- parseParams
                funcbody <- parseDoDone
                return $ Function name params funcbody

parseStatement "function" = parseFunctionCall 

parseStatement _ = fail' "error"

-- Function call is both an expression and a statement.
parseFunctionCall :: Parser String String LAst
parseFunctionCall = do
    name <- parseVarName
    args <- parseArgs
    return $ FunctionCall name args

-- Парсит последовательность statement'ов, разделённых ';'.
parseStatements :: Parser String String LAst
parseStatements = map' Seq $ parseRepeats (keyword keywords >>= parseStatement) statementSeparator

parseL :: Parser String String LAst
parseL = parseStatements

