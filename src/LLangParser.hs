module LLangParser where

import LLang
import Combinators
import Keyword
import Expr
import Control.Applicative
import Data.List

parseLLang = parseStatements

parseStatements = do
    stmnt <- parseStatement
    s' <- parseStatements <|> return (Seq [])
    let (Seq s) = s'
    return $ Seq (stmnt:s)

parseDef :: Parser String String Function
parseDef = do
    spaced $ symbols "def"
    name <- spaced $ parseIdent
    spaced $ symbols "("
    args <- parseArgs
    spaced $ symbols ")"
    spaced $ symbols "{"
    body <- spaced $ parseLLang
    spaced $ symbols "}"
    return $ Function name args body

    
parseArgs = parseArgs' <|> return []

parseArgs' = (do
    a <- spaced parseIdent
    tail <- (spaced (symbols ",") *> parseArgs')
    return (a:tail)
    ) <|> (do a <- spaced parseIdent; return [a])
     

parseProg :: Parser String String Program
parseProg = do
    fs <- parseFuncs
    (Just (Function "main" [] main)) <- return $ find (\f -> name f == "main") fs
    let other = filter (\f -> name f /= "main") fs
    return $ (Program other main)

parseFuncs = do
    f <- spaced parseDef
    fs <- parseFuncs <|> return []
    return (f:fs)
parseStatement = do
   lAst <-  parseAssignment <|> parseWrite <|> parseRead <|> parseWhile <|> parseIf <|> parseReturn

   spaced $ symbols ";"
   return lAst

parseReturn = do
    spaced $ symbols "return"
    returned <- spaced $ parseExpr 
    return $ Return returned

parseAssignment = do
    
    ident <- spaced $ parseIdent
    
    spaced $ spaced $ symbols "="
    
    expr <- spaced $ parseExpr
    return $ Assign ident expr
    
parseWrite = do
    
    spaced $ symbols "write"
    spaced $ symbols "("
    expr <- spaced $ parseExpr

    spaced $ symbols ")"
    return $ Write expr

parseRead = do
    
    spaced $ symbols "read"
    
    spaced $ symbols "("
    ident <- spaced $ parseIdent
    
    spaced $ symbols ")"
    return $ Read ident

parseWhile = do
     
    spaced $ symbols "while"
    
    spaced $ symbols "("
    
    cond <- spaced $ parseExpr
    
    spaced $ symbols ")"
    
    spaced $ symbols "{"
    body <- spaced $ parseStatements
    spaced $ symbols "}"
    return $ While cond body




parseIf = do
     
    spaced $ symbols "if"
    spaced $ symbols "("
    
    expr <- spaced $ parseExpr

    spaced $ symbols ")"
    
    spaced $ symbols "{"
    then' <- spaced $ parseStatements
    
    spaced $ symbols "}"
    
    spaced $ symbols "else"
    
    spaced $ symbols "{"
    else' <- spaced $ parseStatements
    
    spaced $ symbols "}"
    return $ If expr then' else'




