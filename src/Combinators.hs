{-# LANGUAGE LambdaCase #-}

module Combinators where

import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Control.Monad       (ap, liftM2)
import           Data.Char           (digitToInt, isAlpha, isDigit, isSeparator)
import           Debug.Trace         (traceShow)
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

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sepP elP = do
  elem <- elP
  elems <- many $ sepP >> elP
  return $ elem : elems

-- как sepBy1, только еще собирает разделители
sepBy1' :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i (a, [(sep, a)])
sepBy1' sepP elP = do
  elem <- elP
  elems <- many ((,) <$> sepP <*> elP)
  return (elem, elems)

sepBy1'' :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i ([(a, sep)], a)
sepBy1'' sepP elP = do
  elems <- many ((,) <$> elP <*> sepP)
  elem <- elP
  return (elems, elem)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p =
  Parser $ \case
    (x:xs)
      | p x -> Success xs x
    input -> Failure "Predicate failed"

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (== c)

string :: (Eq a) => [a] -> Parser String [a] [a]
string = mapM symbol

digit :: Parser String String Int
digit = digitToInt <$> satisfy isDigit

toNum :: [Int] -> Int
toNum = foldl (\rec x -> x + 10 * rec) 0

nat :: Parser String String Int
nat = toNum <$> some digit

int = nat <|> (negate <$> (negative *> int))
  where
    negative = symbol '-'

eof :: Parser String String ()
eof = Parser result
  where
    result []    = Success [] ()
    result input = Failure "not empty input"

letter :: Parser String String Char
letter = satisfy (not . isSeparator)
  where
    isSeparator ch = ch == ' ' || ch == '\n'

endLine :: Parser String String Char
endLine = symbol '\n'

word :: Parser String String String
word = some letter

space :: Parser String String Char
space = symbol ' '

separator = satisfy isSeparator
  where
    isSeparator ch = ch == ' ' || ch == '\n'