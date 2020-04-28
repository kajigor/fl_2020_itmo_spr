{
module SimpleLex (runSimpleLexer) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

@kw = let | in
@ident = $alpha [$alpha $digit \_ \']*

tokens :-

  $white+ ;
  "--".* ;
  \+      { \s -> TOp Plus }
  \*      { \_ -> TOp Mult }
  \-      { \_ -> TOp Minus }
  \/      { \_ -> TOp Div }
  \=      { \_ -> TEq }
  @kw     { \s -> TKw s }
  \(      { \_ -> TLbr }
  \)      { \_ -> TRbr }
  $digit+ { \s -> TNum (read s) }
  @ident  { \s -> TIdent s }

{
-- Each right-hand side has type :: String -> Token

data Operator = Plus
              | Mult
              | Minus
              | Div
              deriving (Eq, Show)

data Token = TOp    Operator
           | TNum   Int
           | TIdent String
           | TKw    String
           | TLbr
           | TRbr
           | TEq
           deriving (Eq, Show)

runSimpleLexer str =
  let res = alexScanTokens str in
  putStrLn $ unlines $ map show res
}