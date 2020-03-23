module Trie where

import           Combinators         (Parser (..), Result (..), fail')
import           Control.Applicative
import qualified Data.Map            as Map
import           Data.Maybe

newtype Trie =
  Trie
    { children :: Map.Map Char (Bool, Trie)
    }

addString :: String -> Trie -> Trie
addString [] t = t
addString [a] t =
  let kids = children t
   in case getNext t a of
    Just (_, next) -> Trie $ Map.update (\_ -> Just $ (True, next)) a kids
    _ -> insertChar t a True Trie.empty

addString (x:xs) t =
  let kids = children t
   in if containsChar t x
        then Trie $ Map.update (\(s, _) -> Just $ (s, addString xs (snd $ fromJust $ getNext t x))) x kids
        else let next = addString xs (Trie Map.empty)
              in insertChar t x False next

containsString :: String -> Trie -> Bool
containsString [] t = True
containsString [a] t =
  let kids = children t
   in case Map.lookup a kids of
        Just (True, _) -> True
        _              -> False
containsString (x:xs) t =
  let kids = children t
   in case Map.lookup x kids of
        Just (_, next) -> containsString xs next
        _              -> False

containsChar :: Trie -> Char -> Bool
containsChar t c = Map.member c (children t)

getNext :: Trie -> Char -> Maybe (Bool, Trie)
getNext t c = do
  let kids = children t
  (isTerminal, next) <- Map.lookup c kids
  return $ (isTerminal, next)

insertChar :: Trie -> Char -> Bool -> Trie -> Trie
insertChar t c isTerminal next =
  let kids = children t
   in Trie (Map.insert c (isTerminal, next) kids)

empty :: Trie
empty = Trie Map.empty

fromList :: [String] -> Trie
fromList = foldr (\val acc -> addString val acc) Trie.empty

--main :: IO ()
--main = do
--  let a1 = addString "as" Trie.empty
--  let a4 = addString "as?" a1
--  putStrLn $ show $ containsString "as" a1
