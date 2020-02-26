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
{-
  fmap f = Parser . (helper .) . runParser where
    helper (Success input result) = Success input $ f result
    helper (Failure error) = Failure error
-}
  fmap = liftM
instance Applicative (Parser error input) where
{-
  pure x = Parser (\i -> Success i x)
  (Parser pf) <*> (Parser px) = Parser go where
    go inp = case pf inp of
               (Failure err) -> Failure err
               (Success inp' f) -> case px inp' of
                 (Failure err') -> Failure err'
                 (Success inp'' x) -> Success inp'' $ f x
-}
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

digit :: Parser String String Char
digit = satisfy (`elem` "012346789")

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (helper sep elem) where
  helper sep elem = (:) <$> (sep *> elem) <*> helper sep elem <|> pure []

-- Альтернатива: в случае неудачи разбора первым парсером, парсит вторым
alt :: Parser e i a -> Parser e i a -> Parser e i a
alt p q = Parser $ \input ->
  case runParser p input of
    Failure _ -> runParser q input
    x         -> x

-- Последовательное применение парсеров:
-- если первый парсер успешно принимает префикс строки, второй запускается на суффиксе.
-- Второй парсер использует результат первого.
seq' :: Parser e i a -> (a -> Parser e i b) -> Parser e i b
seq' p f = Parser $ \input ->
  case runParser p input of
    Success i a -> runParser (f a) i
    Failure e   -> Failure e

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

-- В случае успешного разбора модифицирует результат при помощи функции f
map' :: (a -> b) -> Parser e i a -> Parser e i b
map' f p = Parser $ \input ->
  case runParser p input of
    Success i a -> Success i (f a)
    Failure e   -> Failure e

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

-- Всегда завершается успехом, вход не читает, возвращает данное значение
return' :: a -> Parser e i a
return' x = Parser $ \input -> Success input x

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Parser e i a -> Parser e i [a]
many' p = some' p `alt` return' []

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Parser e i a -> Parser e i [a]
some' p = p `seq'` \r -> map' (r:) (some' p `alt` return' [])

