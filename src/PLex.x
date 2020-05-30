{
module PLex (runLxr, alexScanTokens, Token (..)) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

@ident = [a-z] [$alpha $digit]*
@var = [A-Z] [$alpha $digit]*

tokens :-
    $white+ ;
    \?\-    { \s -> TTrg }
    \:\-    { \s -> TNeck}
    \,      { \s -> TCom }
    \.      { \s -> TPrd }
    \(      { \s -> TLbr }
    \)      { \s -> TRbr }
    @ident  { \s -> TId s }
    @var    { \s -> TVar s }

{

data Token  = TId String
            | TVar String
            | TTrg
            | TNeck
            | TCom
            | TPrd
            | TLbr
            | TRbr
            deriving (Eq, Show)

runLxr str = 
    let res = alexScanTokens str in
    putStrLn $ unlines $ map show res

}
