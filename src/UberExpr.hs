module UberExpr where

import           Combinators
import           Control.Applicative (Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: (Monoid e, Eq e)
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast


uberExpr xs parseElem opp oppUnary = if (null xs) then parseElem else 
              case (snd $ head xs) of
                Unary -> parseUnary <|> parseTail where
                        parseUnary = do
                            op <- (fst $ head xs)
                            elem <- parseTail
                            return $ oppUnary op elem
                Binary LeftAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseTail
                            return $ foldl (\x (y, z) -> opp y x z) elem expr
                Binary RightAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseTail
                            if (not $ null $ expr) then 
                              let exp = elem : (map snd (init  expr)) in
                              let ops = map fst expr in 
                              return $ foldr (\(x, y) z -> opp x y z) (snd $ last $ expr) (zip ops exp)
                            else return $ elem
                Binary NoAssoc -> parseNoAssoc <|> parseTail where
                         parseNoAssoc = do
                           elem1 <- parseTail
                           op <- (fst $ head xs)
                           elem2 <- parseTail
                           return $ opp op elem1 elem2
              where 
                parseTail = if (null $ tail $ xs) then parseElem 
                            else (uberExpr (tail xs) parseElem opp oppUnary)
