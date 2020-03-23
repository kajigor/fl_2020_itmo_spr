module Keyword where

import           Combinators         (Parser (..), Result (..), fail', satisfy,
                                      symbol)
import           Control.Applicative
import           Trie

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
-- as?
keyword :: [String] -> Parser String String String
keyword ks = parser
  where
    trie = fromList ks
    parser = Parser (parseInput trie)
      where
        parseInput trie xs =
          case go trie xs "" of
            (Just result, rest) -> Success rest (reverse result)
            _                   -> Failure "error"
        go :: Trie -> String -> String -> (Maybe String, String)
        go t [] result = (Nothing, [])
        go t [x] result =
          case getNext t x of
            Just (True, _) -> (Just $ x : result, [])
            _              -> (Nothing, [])
        go t (x:xs) result
          | (head xs == ' ' || head xs == '\n') =
            case getNext t x of
              Just (_, next) ->
                case getNext next ' ' of
                  Just (_, next') ->
                    case go next xs (x : result) of
                      j@(Just res, rest) -> j
                      (Nothing, _) ->
                        case getNext t x of
                          Just (True, next) -> (Just $ x : result, tail xs)
                          Nothing           -> (Nothing, xs)
                  _ -> (Just $ x : result, tail xs)
              _ -> (Nothing, xs)
        go t (x:xs) result =
          case getNext t x of
            Just (_, next) -> go next xs (x : result)
            Nothing        -> (Nothing, xs)
