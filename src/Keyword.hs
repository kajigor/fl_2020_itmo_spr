module Keyword where

import Combinators (Parser (..), Result (..), fail')
import Control.Applicative
import qualified Data.Map.Lazy as M

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
endSymbols = [' ', '\n', '\t', '\r', '\f']

data Trie a = Trie { isCompleted :: Bool
                   , getTrie :: M.Map a (Trie a)
                   } deriving (Eq)

keyword :: [String] -> Parser String String String
keyword ks = Parser $ \input -> fromMaybe (Failure "") (search input [] t)
        where t = buildTrie ks

empty :: Trie a
empty = Trie False M.empty

buildTrie :: Ord a => [a] -> Trie a -> Trie a
buildTrie [] (Trie _ m)     = Trie True m
buildTrie (x:xs) (Trie e m) = Trie e (M.alter (Just . insert xs . fromMaybe empty) x m)

searchTrie :: Ord a => [a] -> Trie a -> Bool
searchTrie [] (Trie e _)     = e
searchTrie (x:xs) (Trie _ m) = fromMaybe False (member xs <$> M.lookup x m)
