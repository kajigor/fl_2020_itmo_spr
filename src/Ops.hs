{-# LANGUAGE FlexibleContexts #-}

module Ops where

import Combinators
import Data.Maybe
import Control.Applicative
import Data.Map
import Keyword
import qualified Data.List as L



op :: [String] -> Parser String String String
op ks = Parser $ \(InputStream i pos) -> fromMaybe (Failure $ makeError [] pos) $ find'' (const True) i [] pos trie  where
    trie = buildTrie ks

    find'' predicate [] acc pos (Trie realNode _) | realNode = Just $ Success (InputStream [] (incrPos' pos l)) Nothing (reverse acc) where
        l = L.length acc
    find'' predicate (y:xs) acc pos (Trie realNode m) | realNode && predicate y =
        ((m !? y) >>= find'' predicate xs (y:acc) pos) <|>  (Just $ Success (InputStream (y:xs) (incrPos' pos l)) Nothing (reverse acc)) where
            l = L.length acc
    find'' predicate (y:xs) acc pos (Trie _ m) = (m !? y) >>= find'' predicate xs (y:acc) pos
    find'' _ _ _ _ _ = Nothing