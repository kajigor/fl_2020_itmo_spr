{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import Control.Monad.Fail
import Control.Applicative
import Data.Char (digitToInt, isDigit, isSpace, isAlpha, isAlphaNum)
import Data.List (sortBy, nub)
import Data.Ord

data Result error input result
  = Success (InputStream input) (Maybe (ErrorMsg error)) result
  | Failure (Maybe (ErrorMsg error))
  deriving (Eq)

newtype Parser error input result
  = Parser { runParser' :: (InputStream input) -> Result error input result }

type Error = String

type Input = String

data Position = Position { line :: Int, col :: Int } deriving (Eq, Show)

instance Ord (Position) where
  a <= b = a == b || (line a < line b) || (line a == line b && col a <= col b)

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq)

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

makeError e p = Just $ ErrorMsg [e] p

initPosition = (Position 0 0)

runParser :: Parser error input result -> input -> Result error input result
runParser parser input = runParser' parser (InputStream input initPosition)

--toStream :: a -> Position -> InputStream a
toStream s n = InputStream s (Position 0 n)

incrPos :: InputStream a -> InputStream a
incrPos (InputStream str pos) = InputStream str (incrPos' pos 1)

incrPos' (Position l c) w = Position l (c+w)

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Success input' es r -> Success input' es (f r)
      Failure e -> Failure e

instance (Monoid error, Eq error) => Applicative (Parser error input) where
  pure result = Parser $ \input -> Success input Nothing result

  f <*> a = f >>= (<$> a)

instance (Monoid error, Eq error) => Monad (Parser error input) where
  return a = Parser $ \input -> Success input Nothing a

  Parser a >>= f = Parser $ \input ->
    case a input of
      Success input' es r ->
        case runParser' (f r) input' of
          Success input'' es' r' -> Success input'' (mergeErrors es es') r'
          Failure e -> Failure (mergeErrors es e)
      Failure e -> Failure e

instance (Monoid error, Eq error) => MonadFail (Parser error input) where
  fail s = Parser $ \input -> Failure (makeError mempty (curPos input))

instance (Monoid error, Eq error) => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure (makeError mempty (curPos input))

  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' es r -> Success input' es r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          (Success i' es' r') -> Success i' (mergeErrors e es') r'

mergeErrors :: (Monoid e, Eq e) => Maybe (ErrorMsg e) -> Maybe (ErrorMsg e) -> Maybe (ErrorMsg e)
mergeErrors Nothing x = x
mergeErrors x Nothing = x
mergeErrors (Just (ErrorMsg e p)) (Just (ErrorMsg e' p')) | p == p' = Just $ ErrorMsg (nub (e <> e')) p
mergeErrors (Just (ErrorMsg e p)) (Just (ErrorMsg e' p')) | p < p'  = Just $ ErrorMsg e' p'
mergeErrors (Just (ErrorMsg e p)) (Just (ErrorMsg e' p')) | p > p'  = Just $ ErrorMsg e p

infixl 1 <?>
(<?>) :: (Monoid error, Eq error) => error -> Parser error input a -> Parser error input a
(<?>) msg (Parser p) = Parser $ \input ->
    case p input of
      Failure (Just err) -> Failure (makeError msg (pos err))
      -- Failure (Just err) -> Failure $ (mergeErrors (Just err) (makeError msg (pos err)))
      -- Failure $ mergeErrors [makeError msg (maximum $ map pos err)] err
      x -> x


terminal :: Char -> Parser Error String Char
terminal = symbol

--  pref :: (Show a, Eq a) => [a] -> Parser Error [a] [a]
pref (w:ws) =
  symbol w >>= \c -> (c:) <$> pref ws

anyOf :: (Monoid error, Eq error) => [Parser error input a] -> Parser error input a
anyOf [] = empty
anyOf (p:ps) = p <|> anyOf ps

digit :: Parser Error String Int
digit = digitToInt <$> satisfy isDigit

number :: Parser Error String Int
number =
    parseNum <$> some (satisfy isDigit)
  where
    parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0

sepBy1L :: (Monoid e, Eq e) => Parser e i sep -> Parser e i elem -> Parser e i (elem, [(sep, elem)])
sepBy1L sep elem = do
    fst <- elem
    rest <- many ((,) <$> sep <*> elem)
    return (fst, rest)

sepBy1R :: (Monoid e, Eq e) => Parser e i sep -> Parser e i elem -> Parser e i ([(elem, sep)], elem)
sepBy1R sep elem = (do
    fst <- many ((,) <$> elem <*> sep)
    lst <- elem
    return (fst, lst))
  <|>
    ((\x -> ([],x)) <$> elem)

sepBy1 :: (Monoid error, Eq error) => Parser error input sep -> Parser error input elem -> Parser error input [elem]
sepBy1 sep elem = do
    (fst, separated) <- sepBy1L sep elem
    return $ fst : (map snd separated)

sepBy :: (Monoid error, Eq error) => Parser error input sep -> Parser error input elem -> Parser error input [elem]
sepBy sep elem = sepBy1 sep elem <|> return []

--satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (InputStream xs (movePos' x pos)) Nothing x
    input        -> Failure (makeError "Predicate failed" pos)

const' :: Char -> Bool
const' _ = True

--elem' :: Parser String [a] a
elem' = satisfy const'

--symbol :: (Eq a, Show a) => a -> Parser String [a] a
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (==c)

fail' :: e -> Parser e i a
fail' msg = Parser $ \input -> Failure (makeError msg (curPos input))

eow :: Parser String String a -> Parser String String a
eow p = p <* (eof <|> space)

eof :: Parser String String ()
eof = Parser $ \input -> if null $ stream input then Success input Nothing () else Failure (makeError "Not eof" (curPos input))

space :: Parser String String ()
space = const () <$> satisfy isSpace

spaced :: Parser String String a -> Parser String String a
spaced p = many space *> p <* many space

parseIdent :: Parser Error Input String
parseIdent = "Failed to parse ident" <?> (:) <$> (satisfy isAlpha <|> symbol '_') <*> (many $ satisfy isAlphaNum <|> symbol '_')

symbols :: String -> Parser Error Input String
symbols w = Parser $ \(InputStream input pos) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (movePos w pos)) Nothing w
  else Failure (makeError ("Expected " ++ show w) pos)

word = symbols

movePos' x (Position l c)| x == '\n' = (Position (l+1) 0)
movePos' x (Position l c)| x == '\t' = (Position l (c+8))
movePos' x (Position l c) = (Position l (c+1))

movePos (x:xs) (Position l c) | x == '\n' = movePos xs (Position (l+1) 0)
movePos (x:xs) (Position l c) | x == '\t' = movePos xs (Position l (c+8))
movePos (x:xs) (Position l c) = movePos xs (Position l (c+1))
movePos _ p = p

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ show e
  show (Success i _ r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i