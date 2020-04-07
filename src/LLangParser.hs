module LLangParser where

import LLang
import Combinators
import Keyword
import Expr
import Control.Applicative


parseLLang = parseStatements

parseStatements = do
    stmnt <- parseStatement
    (Seq s) <- parseStatements <|> return (Seq [])
    return $ Seq (stmnt:s)

parseStatement = do
   lAst <-  parseAssignment <|> parseWrite <|> parseRead <|> parseWhile <|> parseIf
   spaceis
   symbols ";"
   return lAst


parseAssignment = do
    spaceis
    ident <- parseIdent
    spaceis
    symbols "="
    spaceis
    expr <- parseExpr
    return $ Assign ident expr
    
parseWrite = do
    spaceis
    symbols "write"
    spaceis
    symbols "("
    expr <- parseExpr
    spaceis
    symbols ")"
    return $ Write expr

parseRead = do
    spaceis
    symbols "read"
    spaceis
    symbols "("
    ident <- parseIdent
    spaceis
    symbols ")"
    return $ Read ident

parseWhile = do
    spaceis 
    symbols "while"
    spaceis
    symbols "("
    spaceis
    cond <- parseExpr
    spaceis
    symbols ")"
    spaceis
    symbols "{"
    body <- parseStatements
    spaceis
    symbols "}"
    return $ While cond body




parseIf = do
    spaceis 
    symbols "if"
    spaceis
    symbols "("
    spaceis
    expr <- parseExpr
    spaceis
    symbols ")"
    spaceis
    symbols "{"
    then' <- parseStatements
    spaceis
    symbols "}"
    spaceis
    symbols "else"
    spaceis
    symbols "{"
    else' <- parseStatements
    spaceis
    symbols "}"
    return $ If expr then' else'




