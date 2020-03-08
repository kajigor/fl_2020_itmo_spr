module UberExpr where

import           Combinators (Parser (..), sepBy1')
import           Control.Applicative (Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast

uberExpr xs parseElem opp = if (null $ tail $ xs) then do 
             case (snd $ head xs) of
                LeftAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseElem
                            return $ foldl (\x (y, z) -> opp y x z) elem expr
                RightAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseElem
                            if (not $ null $ expr) then 
                              let exp = elem : (map snd (init  expr)) in
                              let ops = map fst expr in 
                              return $ foldr (\(x, y) z -> opp x y z) (snd $ last $ expr) (zip ops exp)
                            else return $ elem
                NoAssoc -> parseNoAssoc <|> parseElem where
                         parseNoAssoc = do
                           elem1 <- parseElem
                           op <- (fst $ head xs)
                           elem2 <- parseElem
                           return $ opp op elem1 elem2
         else do
              case (snd $ head xs) of
                 LeftAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) (uberExpr (tail xs) parseElem opp)
                            return $ foldl (\x (y, z) -> opp y x z) elem expr
                 RightAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) (uberExpr (tail xs) parseElem opp)
                            if (not $ null $ expr) then 
                              let exp = elem : (map snd (init  expr)) in
                              let ops = map fst expr in 
                              return $ foldr (\(x, y) z -> opp x y z) (snd $ last $ expr) (zip ops exp)
                            else return $ elem
                 NoAssoc -> parseNoAssoc <|> (uberExpr (tail xs) parseElem opp) where
                         parseNoAssoc = do
                           elem1 <- (uberExpr (tail xs) parseElem opp)
                           op <- (fst $ head xs)
                           elem2 <- (uberExpr (tail xs) parseElem opp) 
                           return $ opp op elem1 elem2
