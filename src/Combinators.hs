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
  fmap f x = Parser $ \input -> 
    case runParser x input of
        (Success input' result) -> Success input' (f result)
        (Failure error) -> Failure error

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x
  f <*> x = Parser $ \input -> 
    case runParser f input of
      (Success input' g) -> case runParser x input' of
                                    (Success input'' xs) -> (Success input'' (g xs))
                                    (Failure err) -> (Failure err)
      (Failure error) -> Failure error

instance Monad (Parser error input) where
  return x = pure x

  m >>= f = Parser $ \input ->
    case runParser m input of
      (Success input' result) -> runParser (f result) input'
      (Failure error) -> Failure error

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \_ -> Failure mempty

  l <|> r = Parser $ \input ->
      case runParser l input of
          (Failure err) -> runParser r input
          s -> s

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = l <|> r 
  where l = do e <- elem
               _ <- sep
               rest <- sepBy1 sep elem
               return $ e : rest
        r = return <$> elem

sepBy1' :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i (a, [(sep, a)])
sepBy1' sep elem = do e <- elem
                      rest <- many (sep >>= (\s -> elem >>= (\e -> return $ (s, e)))) 
                      return $ (e, rest)

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