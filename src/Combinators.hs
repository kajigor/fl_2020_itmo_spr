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
  return x = pure x

  (Parser p) >>= f = Parser $ \i -> (helper $ p i)  where
    helper (Success i a) = runParser (f a) i 
    helper (Failure err) = Failure err


instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \_ -> Failure mempty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Failure e1  -> case p2 input of
                    Failure e2 -> Failure e2
                    x          -> x
    x           -> x

-- instance Monad (Parser error input) => MonadError (Parser error input) where
--     throwError 
  
-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
--sepBy1 :: Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = do
  a <- elem
  res <- (sep *> sepBy1 sep elem) <|> return []
  return (a:res)

sepBy1' sep elem = do
  a <- elem
  xs <- many' (sep >>= (\s -> fmap (\l -> (s, l)) elem)) <|> return []
  return $ (a, xs)

-- Альтернатива: в случае неудачи разбора первым парсером, парсит вторым
alt :: Parser e i a -> Parser e i a -> Parser e i a
alt p q = Parser $ \input ->
  case runParser p input of
    Failure _ -> runParser q input
    x         -> x

-- -- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
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
some' p = p >>= (\r -> fmap (r:) (some' p `alt` return' []))

