module UberExpr where

import           Combinators (Parser (..), sepBy1')
import           Control.Applicative (Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast

uberExpr xs parseElem opp = if (null xs) then parseElem else 
              case (snd $ head xs) of
                LeftAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseTail
                            return $ foldl (\x (y, z) -> opp y x z) elem expr
                RightAssoc -> do
                            (elem, expr) <- sepBy1' (fst $ head xs) parseTail
                            if (not $ null $ expr) then 
                              let exp = elem : (map snd (init  expr)) in
                              let ops = map fst expr in 
                              return $ foldr (\(x, y) z -> opp x y z) (snd $ last $ expr) (zip ops exp)
                            else return $ elem
                NoAssoc -> parseNoAssoc <|> parseElem where
                         parseNoAssoc = do
                           elem1 <- parseTail
                           op <- (fst $ head xs)
                           elem2 <- parseTail
                           return $ opp op elem1 elem2
              where 
                parseTail = if (null $ tail $ xs) then parseElem 
                            else (uberExpr (tail xs) parseElem opp)
        
