module Keyword where

import           Combinators         (InputStream (..), Parser (..),
                                      Result (..), elem', endLine, eof, fail',
                                      makeError, space)
import           Control.Applicative
import           Control.Monad       (guard, void)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)

data Trie c =
  Node
    { isTerminal  :: Bool
    , getChildren :: M.Map c (Trie c)
    }
  deriving (Show)

type PrefixTrie = Trie Char

emptyTrie :: PrefixTrie
emptyTrie = Node False M.empty

insert :: String -> PrefixTrie -> PrefixTrie
insert (x:xs) trie =
  let children = getChildren trie
      node = fromMaybe emptyTrie $ M.lookup x children
   in trie {getChildren = M.insert x (insert xs node) children}
insert [] (Node _ dict) = Node True dict

contains :: String -> PrefixTrie -> Bool
contains (x:xs) trie =
  let children = getChildren trie
      node = M.lookup x children
   in maybe False (contains xs) node
contains [] node = isTerminal node

trieStep :: Char -> PrefixTrie -> Maybe PrefixTrie
trieStep x trie = M.lookup x $ getChildren trie

fromList :: [String] -> PrefixTrie
fromList = foldr insert emptyTrie

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
keyword :: [String] -> Parser String String String
keyword ks = parser trie ""
  where
    parser :: PrefixTrie -> String -> Parser String String String
    parser tree acc = do
      x' <- peek
      guard $ M.member x' $ getChildren tree
      x <- elem'
      case trieStep x tree of
        Just node
          | isTerminal node ->
            let acc' = acc ++ [x]
                parser' = parser node acc'
             in parser' <|> (parser' <* separator) <|> (separator *> parser') <|> (acc' <$ (separator <|> eof))
          | otherwise -> parser node (acc ++ [x])
        Nothing
          | null acc -> fail' ""
        Nothing -> return acc
    trie = fromList ks
    separator = void $ some $ space <|> void endLine
    peek :: Parser String String Char
    peek =
      Parser $ \(InputStream input pos) ->
        case input of
          (x:xs) -> Success (InputStream input pos) Nothing x
          [] -> Failure (makeError "Empty string is not expected" (curPos (InputStream input pos)))
