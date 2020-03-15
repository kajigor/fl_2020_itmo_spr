module Keyword where

import           Prelude hiding (lookup)
import           Combinators (Parser (..), Result (..), fail', symbol, eof)
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Map (Map)

-- Парсер ключевых слов: принимает список ключевых слов,
-- проверяет, что вход начинается с ключевого слова.
-- После ключевого слова во входе должен быть пробельный символ или конец строки.
-- Должен строить по входным ключевым словам либо минимальный автомат, либо бор.
-- Если префикс входа длиной n не является префиксом ни одного входного ключевого слова, чтение n+1-ого символа проводиться не должен.
keyword :: [String] -> Parser String String String
keyword ks = do
  res <- trieParser
  eof <|> void (symbol ' ') <|> void (symbol '\n')
  pure res
  where
    trie = foldr (\ks -> insert ks ks) emptyTrie ks
    trieParser = Parser (go trie)

    go (Branch mbValue children) [] =
      case mbValue of
        Just path -> Success [] path
        Nothing -> Failure ""
    go (Branch mbValue children) input@(x:xs) =
      case mbValue of
        Just value ->
          -- try longer path, return current value if failed
          case M.lookup x children of
            Just trie ->
              case go trie xs of
                Failure _ -> Success input value
                s -> s
            Nothing -> Success input value

        Nothing ->
          case M.lookup x children of
            Just trie ->
              go trie xs
            Nothing -> Failure ""

data Trie k v = Branch (Maybe v) (Map k (Trie k v))

emptyTrie :: Trie k v
emptyTrie = Branch Nothing M.empty

insert
  :: Ord k
  => [k]
  -> v
  -> Trie k v
  -> Trie k v
insert path value (Branch mbOldValue children) =
  case path of
    [] -> Branch (Just value) children
    (head : tail) ->
      let child =
            case M.lookup head children of
              Just trie ->
                insert tail value trie
              Nothing ->
                mkArc tail $ Branch (Just value) $ M.empty
      in
        Branch mbOldValue $ M.insert head child children

mkArc :: [k] -> Trie k v -> Trie k v
mkArc [] trie = trie
mkArc (x:xs) trie =
  Branch Nothing $ M.singleton x (mkArc xs trie)
