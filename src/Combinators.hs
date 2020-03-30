{-# LANGUAGE DeriveFunctor #-}
module Combinators where

import           Control.Monad (ap)
import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Text.Printf         (printf)

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq, Functor)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result}
  deriving (Functor)

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x
  (<*>) = ap

instance Monad (Parser error input) where
  return = pure

  p >>= f = Parser $ \input ->
    case runParser p input of
      Success i a -> runParser (f a) i
      Failure e   -> Failure e

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure mempty
  p <|> q = Parser $ \input ->
    case runParser p input of
      Failure _ -> runParser q input
      x         -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = do
  x <- elem
  xs <- many (sep *> elem)
  pure $ x : xs

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    input        -> Failure $ "Predicate failed"

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

eof :: Parser String [a] ()
eof = Parser $ \input -> case input of
  [] -> Success [] ()
  _ -> Failure "Not an EOF"

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (==c)

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e
