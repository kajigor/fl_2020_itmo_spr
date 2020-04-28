{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module Grammar where

import           Data.List   (intercalate, partition)
import qualified Data.Map    as Map
import           Text.Printf (printf)

data Grammar term nonterm = Grammar
  { terms        :: [term]
  , nonterms     :: [nonterm]
  , rules        :: Rules term nonterm
  , startNonterm :: nonterm
  }

data Symbol term nonterm
  = Term term
  | Nonterm nonterm
  deriving (Eq, Ord)

newtype Rhs term nonterm = Rhs { unRhs :: [Symbol term nonterm] }

newtype Rules term nonterm = Rules { unRules :: Map.Map nonterm [Rhs term nonterm] }

isTerm :: Symbol t n -> Bool
isTerm (Term _) = True
isTerm _ = False

fromTerm :: Symbol t n -> t
fromTerm (Term t) = t

isNonterm :: Symbol t n -> Bool
isNonterm (Nonterm _) = True
isNonterm _ = False

-- Simulates one step of derivation: substitutes every possible production for the first nonterminal
oneStepDerivation :: (Ord n) => Grammar t n -> Rhs t n -> Maybe [Rhs t n]
oneStepDerivation g (Rhs syms) =
  let (terms, rest) = span isTerm syms in
  case rest of
    [] -> Just [Rhs terms]
    (Nonterm n : ss) ->
      case Map.lookup n (unRules $ rules g) of
        Nothing -> Nothing
        Just rhss -> Just $ [ Rhs (terms ++ unRhs rhs ++ ss) | rhs <- rhss ]

-- Bruteforce recognition: works only for epsilon-free grammars.
-- EXTREMELY slow!
recognize :: (Ord n, Eq t, Show t, Show n) => Grammar t n -> [t] -> Bool
recognize g str =
    go (Rhs [Nonterm $ startNonterm g])
  where
    go symbols =
      case oneStepDerivation g symbols of
        Nothing -> False
        Just xs ->
          let (withNonterms, onlyTerminals) = partition (any isNonterm . unRhs) xs in
          if any isInput onlyTerminals
          then True
          else
            let toDerive = filter (not . tooLong) withNonterms in
            or $ map go toDerive
    tooLong (Rhs x) = length x > length str
    isInput (Rhs x) = x == map Term str

-- Example grammars
grammar0 =
    Grammar { terms = ['(', ')'], nonterms = ['S'], rules = Rules rules, startNonterm = 'S'}
  where
    rules = Map.fromList [('S', [ Rhs []
                                , Rhs [Term '(', Nonterm 'S', Term ')', Nonterm 'S']]
                         )]

-- You can use custom datatypes for terms and nonterms
data TermExmpl = Lbr | Rbr deriving (Eq, Ord)
data NontermExmpl = S deriving (Eq, Ord)

grammar1 =
    Grammar { terms = [Lbr, Rbr], nonterms = [S], rules = Rules rules, startNonterm = S}
  where
    rules = Map.fromList [(S, [ Rhs []
                              , Rhs [Term Lbr, Nonterm S, Term Rbr, Nonterm S]]
                         )]

instance Show TermExmpl where
  show Lbr = "("
  show Rbr = ")"

instance Show NontermExmpl where
  show S = "S"

-- Strings can be nonterminals; terminals can have different data types than nonterminals
grammar2 =
    Grammar { terms = ['0' .. '9'] ++ ['+', '-', '*', '/', '(', ')']
            , nonterms = ["Expr", "Term", "Factor", "Num"]
            , rules = Rules rules
            , startNonterm = "Expr"
            }
  where
    rules = Map.fromList  [ ("Expr",  [ Rhs [Nonterm "Term"]
                                      , Rhs [Nonterm "Expr", Term '+', Nonterm "Term"]
                                      , Rhs [Nonterm "Expr", Term '-', Nonterm "Term"]
                                      ]
                            )
                          , ("Term",  [ Rhs [Nonterm "Factor"]
                                      , Rhs [Nonterm "Term", Term '*', Nonterm "Factor"]
                                      , Rhs [Nonterm "Term", Term '/', Nonterm "Factor"]
                                      ]
                            )
                          , ("Factor",  [ Rhs [Nonterm "Num"]
                                        , Rhs [Term '(', Nonterm "Expr", Term ')']
                                        ]
                            )
                          , ("Num", map Rhs $ digit ++ (map (Nonterm "Num" :) digit))
                          ]
    digit = map ((:[]) . Term) ['0' .. '9']

-- Parenthesis grammar with no epsilon rules
grammar3 =
    Grammar { terms = ['(', ')'], nonterms = ['S'], rules = Rules rules, startNonterm = 'S'}
  where
    rules = Map.fromList [('S', [ Rhs [Term '(', Term ')']
                                , Rhs [Term '(', Term ')', Nonterm 'S']
                                , Rhs [Term '(', Nonterm 'S', Term ')']
                                , Rhs [Term '(', Nonterm 'S', Term ')', Nonterm 'S']
                                ]
                          )]


instance (Show term, Show nonterm) => Show (Symbol term nonterm) where
  show (Term x) = show x
  show (Nonterm x) = show x

instance (Show term, Show nonterm) => Show (Rhs term nonterm) where
  show (Rhs []) = "\\eps"
  show (Rhs xs) =
      unwords $ map show $ xs

instance (Show term, Show nonterm) => Show (Rules term nonterm) where
  show =
      unlines . map showRule . Map.toList . unRules
    where
      showRule (n, rhs) = printf "%s -> %s" (show n) (intercalate " | " $ map show rhs)

instance (Show term, Show nonterm) => Show (Grammar term nonterm) where
  show (Grammar terms nonterms rules startNonterm) =
    printf "Terms:\n%s\nNonterms:\n%s\nStart nonterminal:\n%s\n\nRules:\n%s"
           (unlines $ map show terms)
           (unlines $ map show nonterms)
           (show startNonterm)
           (show rules)