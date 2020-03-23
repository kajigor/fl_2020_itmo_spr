{-# LANGUAGE FlexibleContexts #-}

module Ops where

import Combinators (Parser (..), Result (..), fail')
import Data.Maybe
import Control.Applicative
import Data.Map
import Keyword


op :: [String] -> Parser String String String
op ks = Parser $ \i -> fromMaybe (Failure "") $ find'' (const True) i [] trie  where
    trie = buildTrie ks

find'' predicate [] acc (Trie realNode _) | realNode = Just $ Success [] (reverse acc)
find'' predicate (y:xs) acc (Trie realNode m) | realNode && predicate y =
    ((m !? y) >>= find'' predicate xs (y:acc)) <|>  (Just $ Success (y:xs) (reverse acc))
find'' predicate (y:xs) acc (Trie _ m) = (m !? y) >>= find'' predicate xs (y:acc)
find'' _ _ _ _ = Nothing