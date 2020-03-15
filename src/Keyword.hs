module Keyword where

import Combinators (Parser(..), Result(..), fail', satisfy, symbol)
import Control.Applicative
import Control.Monad (guard)

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
data Trie
  = Empty
  | Node Bool [(Char, Trie)]
  deriving (Show)

insert :: String -> Trie -> Trie
insert "" Empty = Node True []
insert (x:xs) Empty = Node False [(x, insert xs Empty)]
insert "" (Node _ edges) = Node True edges
insert (x:xs) (Node bool edges) =
  let edges' = findedge x edges []
   in case edges' of
        (x', node):rest
          | x' == x -> Node bool ((x', insert xs node) : rest)
        _ -> Node bool ((x, insert xs Empty) : edges')

-- reshuffles edges so that the desired one is at the beginning of the list
findedge :: Char -> [(Char, Trie)] -> [(Char, Trie)] -> [(Char, Trie)]
findedge ch [] acc = acc
findedge ch (tup@(x, node):xs) acc
  | x == ch = tup : (acc ++ xs)
  | otherwise = findedge ch xs (tup : acc)

checkword :: String -> Trie -> Bool
checkword _ Empty = False
checkword "" (Node bool edges) = bool
checkword (x:xs) (Node bool edges) =
  let edges' = findedge x edges []
   in case edges' of
        (ch, node):rest -> (x == ch) && checkword xs node
        [] -> False

fromList :: [String] -> Trie -> Trie
fromList [] acc = acc
fromList (x:xs) acc = fromList xs (insert x acc)

getnext :: Trie -> Parser String String String
getnext Empty = empty
getnext (Node bool edges) = parser <|> parser2 <|> parser3
  where
    parser = do
      x <- satisfy (const True)
      case findedge x edges [] of
        (x', node):etc
          | x == x' -> do
            rest <- getnext node
            return (x : rest)
        _ -> empty
    parser2 = do
      guard bool
      satisfy (\x -> x == ' ' || x == '\n')
      return ""
    parser3 = do
      guard bool
      return ""

keyword :: [String] -> Parser String String String
keyword ks = getnext (fromList ks Empty)
