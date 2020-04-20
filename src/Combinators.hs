{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import Data.Char (digitToInt, isDigit, isSpace, isAlpha, isAlphaNum)
import Data.List (sortBy, nub)
import           AST                 (AST (..), Operator (..))
import           Control.Applicative (Alternative (..))
import           Text.Printf         (printf)
import           Control.Monad (liftM, ap)

data Result error input result
  = Success (InputStream input) (Maybe (ErrorMsg error)) result
  | Failure (Maybe (ErrorMsg error))
  deriving (Eq)

newtype Parser error input result
  = Parser { runParser' :: (InputStream input) -> Result error input result }

type Error = String

type Input = String

data Position = Position {line :: Int, offset :: Int} deriving (Show, Eq)

instance Ord Position where
  (Position l1 o1) <= (Position l2 o2) = (l1 <= l2) && (o1 <= o2)

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq)

class StreamSymbol b where
  incrPos :: b -> InputStream a -> InputStream a
  incrPos char (InputStream str (Position l o)) = InputStream str $ Position l (o + 1)

instance StreamSymbol Char where
  incrPos char (InputStream str (Position l o)) =
    case char of
      '\n' -> InputStream str (Position (l + 1) 0)
      '\t' -> InputStream str (Position l (o + 8))
      _   -> InputStream str (Position l (o + 1))

instance StreamSymbol Integer

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

makeError e p = Just $ ErrorMsg [e] p

initPosition = Position 1 0

runParser :: Parser error input result -> input -> Result error input result
runParser parser input = runParser' parser (InputStream input initPosition)

toStream :: a -> Position -> InputStream a
toStream = InputStream

toStreamInitPos str = InputStream str initPosition

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

instance (Monoid error, Eq error) => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure (makeError mempty (curPos input))

  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' es r -> Success input' es r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          (Success i' es' r') -> Success i' (mergeErrors e es') r'

getPos :: Parser error input Position
getPos = Parser $ \input -> Success input Nothing (curPos input)

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

pref :: (Show a, Eq a, StreamSymbol a) => [a] -> Parser Error [a] [a]
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

satisfy :: StreamSymbol a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (incrPos x (InputStream xs pos)) Nothing x
    input        -> Failure (makeError "Predicate failed" pos)

elem' :: StreamSymbol a => Parser String [a] a
elem' = satisfy (const True)

symbol :: (Eq a, Show a, StreamSymbol a) => a -> Parser String [a] a
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

word :: String -> Parser Error Input String
word w = Parser $ \(InputStream input pos@(Position l o)) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (Position l $ o + length w)) Nothing w
  else Failure (makeError ("Expected " ++ show w) pos)

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ show e
  show (Success i _ r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i
