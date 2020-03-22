module Combinators where

import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Text.Printf         (printf)
import           Control.Monad (liftM, ap)

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result}

instance Functor (Parser error input) where
  fmap = liftM

instance Applicative (Parser error input) where
  pure = return
  (<*>) = ap

instance Monad (Parser error input) where
  return x = Parser (\i -> Success i x)
  (Parser m) >>= k = Parser go where
    go inp = case m inp of
               (Failure err) -> (Failure err)
               (Success inp' x) -> runParser (k x) inp'

instance Monoid error => Alternative (Parser error input) where
  empty = Parser (\i -> Failure mempty)
  (Parser p1) <|> (Parser p2) = Parser go where
    go inp = case p1 inp of
               (Failure _) -> p2 inp
               s@(Success _ _) -> s

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (helper sep elem) where
  helper sep elem = (:) <$> (sep *> elem) <*> helper sep elem <|> pure []

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    input        -> Failure $ "Predicate failed"

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (==c)

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e
