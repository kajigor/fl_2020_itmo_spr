{-# LANGUAGE FlexibleContexts #-}

module Keyword where

import Combinators (Parser (..), Result (..), fail')
import Control.Applicative
import Data.Map as M
import Data.List as L
import Data.Char
import Data.Maybe

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
keyword :: [String] -> Parser String String String
keyword ks = Parser $ \i -> fromMaybe (Failure "") $ find' i [] trie  where
    trie = buildTrie ks

data Trie = Trie Bool (Map Char Trie)

groupBy' f l = go l M.empty where
    go [] m = m
    go (x:xs) m = go xs $ M.insert (f x) (x:findWithDefault [] (f x) m) m

removeEmpty l = L.foldr go [] l where
    go x acc | L.null x = acc 
    go x acc = x:acc

buildTrie ks = Trie (elem "" ks) childs where
    grouped = groupBy' head $ removeEmpty ks
    childs = M.map (\l -> buildTrie $ fmap tail l) grouped 


find' [] acc (Trie realNode _) | realNode = Just $ Success [] (reverse acc)
find' (y:xs) acc (Trie realNode m) | realNode && isSpace y =
    ((m !? y) >>= find' xs (y:acc)) <|>  (Just $ Success xs (reverse acc))
find' (y:xs) acc (Trie _ m) = (m !? y) >>= find' xs (y:acc)
find' _ _ _ = Nothing