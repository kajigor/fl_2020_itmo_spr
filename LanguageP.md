Синтаксис языка P.

Пробелы и переносы строк между лексемами не являются значимыми.



P :: Relations ?- Target

Relations :: Empty | Relation | Relation Relations 

Relation ::  Head . | Head :- Body

Head :: Atom

Args :: Arg | Arg, Args

Arg :: Var | Atom

Atom :: Ident | Ident (Args)

LetterSmall :: 'a' | ... | 'z' 

LetterBig :: 'A' | ... | 'Z'

Digit :: '0' | ... | '9'

Id :: LetterSmall | LetterBig | Digit | LetterSmall Id | LetterBig Id | Digit Id

Var :: LetterBig Id

Ident :: LetterSmall Id

Atoms :: Atom . | Atom, Atoms

Target :: Atoms

Body :: Atoms



