{
module Lex (Token(..), Operator(..), AlexPosn(..), alexScanTokens, token_posn, runLexer, showPos) where
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

@kw = let | in
@ident = $alpha [$alpha $digit \_ \’]*

tokens :-

  $white+ ;
  "--".* ;
  \+      { \p _ -> TOp p Plus }
  \*      { \p _ -> TOp p Mult }
  \-      { \p _ -> TOp p Minus }
  \/      { \p _ -> TOp p Div }
  \=      { \p _ -> TEq p }
  @kw     { \p s -> TKw p s }
  \(      { \p _ -> TLbr p }
  \)      { \p _ -> TRbr p }
  $digit+ { \p s -> TNum p (read s) }
  @ident  { \p s -> TIdent p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

data Operator = Plus
              | Mult
              | Minus
              | Div
              deriving (Eq, Show)

data Token = TOp    AlexPosn Operator
           | TNum   AlexPosn Int
           | TIdent AlexPosn String
           | TKw    AlexPosn String
           | TLbr   AlexPosn
           | TRbr   AlexPosn
           | TEq    AlexPosn
           deriving (Eq, Show)

runLexer str =
  let res = alexScanTokens str in
  putStrLn $ unlines $ map show res

token_posn (TOp    p _) = p
token_posn (TNum   p _) = p
token_posn (TIdent p _) = p
token_posn (TKw    p _) = p
token_posn (TLbr   p)   = p
token_posn (TRbr   p)   = p
token_posn (TEq    p)   = p

showPos :: AlexPosn -> String
showPos (AlexPn _ ln col) = "line: " ++ show ln ++ ", col: " ++ show col
}