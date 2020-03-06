module Combinators where

import           Control.Monad (liftM, ap)
import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Text.Printf         (printf)

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result}

instance Functor (Parser error input) where
--map'
  fmap f p = Parser $ \input ->
  	case runParser p input of
    	Success i a -> Success i (f a)
    	Failure e   -> Failure e

instance Applicative (Parser error input) where
  pure = return
  (<*>) = ap

instance Monad (Parser error input) where
--return'
  return x = Parser $ \input -> Success input x
--seq'
  (>>=) p f = Parser $ \input ->
  	case runParser p input of
    		Success i a -> runParser (f a) i
    		Failure e   -> Failure e

instance Monoid error => Alternative (Parser error input) where
--fail'
  empty = Parser $ \input -> Failure mempty

  (<|>) p q = Parser $ \input ->
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
	xs <- many' (sep >> elem)
	return (x:xs)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    input        -> Failure $ "Predicate failed"

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Eq a) => a -> Parser String [a] a
symbol c = satisfy (==c)

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Monoid e => Parser e i a -> Parser e i [a]
many' p = some' p <|> return []

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Monoid e => Parser e i a -> Parser e i [a]
some' p = p >>= \r -> fmap (r:) (some' p <|> return [])



