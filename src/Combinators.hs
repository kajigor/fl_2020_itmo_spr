module Combinators where

import Control.Monad.Fail
import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Text.Printf         (printf)
import Data.Char
data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result}

instance Functor (Parser error input) where
  fmap f (Parser parser) = Parser (helper . parser) where
    helper (Success i r) = Success i (f r)
    helper (Failure err) = Failure err 

instance Applicative (Parser error input) where
  pure x = Parser (\i -> Success i x)
  (Parser p1) <*> (Parser p2) = Parser $ helper . p1 where
    helper (Success i f) = helper' f (p2 i)
    helper (Failure err) = Failure err
    helper' f (Success i a) = Success i (f a)
    helper' _ (Failure err) = Failure err
    

instance Monad (Parser error input) where
  return = pure

  (Parser p) >>= f = Parser $ \i -> helper $ p i  where
    helper (Success i a) = runParser (f a) i 
    helper (Failure err) = Failure err

instance Monoid error => MonadFail (Parser error input) where
  fail s = Parser $ \_ -> Failure mempty

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \_ -> Failure mempty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Failure e1  -> case p2 input of
                    Failure e2 -> Failure e2
                    x          -> x
    x           -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 sep elem = do
  a <- elem
  xs <- (sep >>= \_-> sepBy1 sep elem) <|> return []
  return $ a:xs

sepBy1L sep elem = do
  a <- elem
  xs <- many (sep >>= (\s -> fmap (\l -> (s, l)) elem)) <|> return []
  return $ (a, xs)

sepBy1R sep elem = do
  xs <- many (elem >>= (\el -> fmap (\s -> (el, s)) sep)) <|> return []
  a <- elem
  return (xs, a)

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

symbols s = Parser $ \i ->  helper s i where
  helper (x:xs) (y:ys) | x == y  = helper xs ys
  helper [] ys = Success ys s
  helper _ _ = Failure ""

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

whiteSpace = Parser $ \i -> helper i where
  helper (x:xs) | isSpace x = Success xs ()
  helper _ = Failure ""

spaceis = Parser helper where
  helper (x:xs) | isSpace x = Success (dropWhile isSpace xs) ()
  helper xs = Success xs ()