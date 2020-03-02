module Combinators where

import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Control.Monad       (ap)
import           Data.Char           (digitToInt, isDigit)
import           Text.Printf         (printf)

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result =
  Parser
    { runParser :: input -> Result error input result
    }

instance Functor (Parser error input) where
  fmap g (Parser p) = Parser f
    where
      f input =
        case p input of
          (Success input' result') -> Success input' (g result')
          (Failure error)          -> Failure error

instance Applicative (Parser error input) where
  pure = return
  (<*>) = ap

instance Monad (Parser error input) where
  return x = Parser $ \input -> Success input x
  (Parser x) >>= k = Parser f
    where
      f input =
        case x input of
          Success input' result' -> runParser (k result') input'
          Failure error          -> Failure error

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure mempty
  Parser u <|> Parser v = Parser f
    where
      f input =
        case u input of
          Failure _ -> v input
          x         -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sepP elP = do
  elem <- elP
  elems <- many $ sepP >> elP
  return $ elem : elems

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p =
  Parser $ \input ->
    case input of
      (x:xs)
        | p x -> Success xs x
      input -> Failure "Predicate failed"

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (== c)

digit :: Parser String String Int
digit = digitToInt <$> satisfy isDigit

toNum :: [Int] -> Int
toNum = foldl (\rec x -> x + 10 * rec) 0

nat :: Parser String String Int
nat = toNum <$> some digit
