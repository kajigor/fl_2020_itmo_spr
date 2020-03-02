module Combinators where

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
  fmap f p =  Parser $ \input ->
        case runParser p input of
            Success i a -> Success i (f a)
            Failure e   -> Failure e

instance Applicative (Parser error input) where
  pure = return
  (<*>) p1 p2 = Parser $ \input ->
         case runParser p1 input of
            Success i a -> runParser (a <$> p2) i                           
            Failure e   -> Failure e

instance Monad (Parser error input) where
  return x = Parser (\input -> Success input x)
  (>>=) p f = Parser $ \input ->
       case runParser p input of
           Success i a -> runParser (f a) i
           Failure e   -> Failure e


instance Monoid error => Alternative (Parser error input) where
  empty = Parser (\input -> Failure mempty)
  (<|>) p q = Parser $ \input ->
       case runParser p input of
           Failure _ -> runParser q input
           x         -> x


-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> many' p
         where 
           p = sep *> elem


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

-- Всегда завершается успехом, вход не читает, возвращает данное значение
return' :: a -> Parser e i a
return' x = Parser $ \input -> Success input x

--Последовательное применение одного и того же парсера 0 или более раз
many' :: Monoid e => Parser e i a -> Parser e i [a]
many' p = some' p <|> return []

--Последовательное применения одного и того же парсера 1 или более раз
some' :: Monoid e => Parser e i a -> Parser e i [a]
some' p = p >>= \r -> (r:) <$> (some' p <|> return [])

