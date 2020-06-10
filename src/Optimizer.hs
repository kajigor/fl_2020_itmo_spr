module Optimizer where


import           AST                            
import UberExpr
import           Control.Applicative
import           Expr                           
import           Combinators                                                                    

parserSumMult :: Parser String String AST
parserSumMult =
     uberExpr [ (sum', Binary LeftAssoc)
              , (mult, Binary LeftAssoc)
              ]
              ((Num <$> parseNatural) <|> (Ident <$> Expr.parseIdent) <|> (symbol '(' *> parserSumMult <* symbol ')') )
              BinOp
              UnaryOp

evaluateOptimized :: String -> Maybe AST
evaluateOptimized input = case runParser parserSumMult input of
    Success rest _ ast | null (stream rest) -> return $ optimize ast
    _ -> Nothing

optimize :: AST -> AST
optimize a@(Num   _       ) = a
optimize a@(Ident _       ) = a
optimize (  BinOp Plus l r) = case left of
    Num i -> if i == 0 then right else case right of
        Num j -> Num (i + j)
        BinOp Plus l'@(Num j) r' ->
            if j == 0 then Num i else BinOp Plus (Num (i + j)) r'
        BinOp Plus l' r'@(Num j) ->
            if j == 0 then Num i else BinOp Plus l' (Num (i + j))
        _ -> if i == 0 then right else BinOp Plus left right
    _ -> case right of 
        Num 0 -> left
        _ -> BinOp Plus left right
  where
    left  = optimize l
    right = optimize r

optimize (BinOp Mult l r) = case left of
    Num i -> if i == 0
        then Num 0
        else if i == 1
            then right
            else case right of
                Num j -> Num (i * j)
                BinOp Mult l'@(Num j) r' ->
                    if j == 0 then Num 0 else BinOp Mult (Num (i * j)) r'
                BinOp Mult l' r'@(Num j) ->
                    if j == 0 then Num 0 else BinOp Mult l' (Num (i * j))

                _ -> if i == 0 then Num 0 else BinOp Mult left right
    Ident i -> case right of
        n@(Num j) ->
            if j == 0 then Num 0 else if j == 1 then left else BinOp Mult left n
        BinOp Mult l'@(Num j) r' -> if j == 0
            then Num 0
            else if j == 1 then BinOp Mult left r' else BinOp Mult left right
        BinOp Mult l' r'@(Num j) -> if j == 0
            then Num 0
            else if j == 1 then BinOp Mult left l' else BinOp Mult left right
        _ -> BinOp Mult left right

    _ -> case right of
        Num 0 -> Num 0
        _ -> BinOp Mult left right
  where
    left  = optimize l
    right = optimize r

