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
  fmap f prsr = let res = runParser prsr
    in Parser $ \inpt -> case res inpt of
        Success input result -> Success input (f result)
        Failure error -> Failure error

instance Applicative (Parser error input) where
  pure x = Parser $ \inpt -> Success inpt x  
  (Parser fprsr) <*> (Parser prsr) = Parser $ \inpt ->
    case fprsr inpt of
      Success input fresult -> case prsr input of
                                Success i r -> Success i (fresult r)
                                Failure er -> Failure er
      Failure err -> Failure err

instance Monad (Parser error input) where
  return = pure
  (Parser fprsr) >>= f = Parser $ \inpt ->
    case fprsr inpt of 
      Success input result -> runParser (f result) input
      Failure error -> Failure error

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure mempty

  (Parser a) <|> (Parser b) = Parser $ \inpt ->
    case a inpt of
      Success input result -> Success input result
      Failure error -> b inpt

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = a <|> some elem
  where a = do
          el <- elem
          sep
          elems <- sepBy1 sep elem
          return (el:elems)
  

-- Альтернатива: в случае неудачи разбора первым парсером, парсит вторым
alt :: Monoid e => Parser e i a -> Parser e i a -> Parser e i a
alt = (<|>)

-- Последовательное применение парсеров:
-- если первый парсер успешно принимает префикс строки, второй запускается на суффиксе.
-- Второй парсер использует результат первого.
seq' :: Parser e i a -> (a -> Parser e i b) -> Parser e i b
seq' = (>>=)

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
map' = fmap

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

-- Всегда завершается успехом, вход не читает, возвращает данное значение
return' :: a -> Parser e i a
return' = return

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Monoid e => Parser e i a -> Parser e i [a]
many' p = some' p <|> return' []

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Monoid e => Parser e i a -> Parser e i [a]
some' p = do
  r <- p
  map' (r:) (some' p <|> return' [])

  -- p >>= \r -> map' (r:) (some' p <|> return' [])
empty' :: Monoid e => Parser e i a
empty' = empty