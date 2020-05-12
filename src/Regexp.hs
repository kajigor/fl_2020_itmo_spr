module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char _) = False
nullable (Alt p q) = nullable p || nullable q
nullable (Seq p q) = nullable p && nullable q
nullable (Star _) = True

derivative :: Char -> Regexp -> Regexp
derivative _ Empty = empty
derivative _ Epsilon = empty
derivative a (Char c) | a == c = epsilon
derivative _ (Char _) = empty
derivative a (Alt p q) = alt (derivative a p) (derivative a q)
derivative a (Seq p q) | not (nullable p) = seq (derivative a p) q
derivative a (Seq p q) = alt (seq (derivative a p) q) (derivative a q)
derivative a (Star p) = seq (derivative a p) (star p)

empty = Empty
epsilon = Epsilon
char = Char

-- Empty . p = Empty
seq Empty _ = Empty
seq _ Empty = Empty
-- Epsilon . p = p
seq Epsilon p = p
seq p Epsilon = p
seq p q = Seq p q

-- Empty | p = p
alt Empty p = p
alt p Empty = p
alt Epsilon p = if nullable p then p else Alt Epsilon p
alt p Epsilon = if nullable p then p else Alt Epsilon p
-- p | p = p
alt p q | p == q = p
alt p q = Alt p q

star Empty = Epsilon
star Epsilon = Epsilon
-- p** = p*
star (Star p) = Star p
star p = Star p
