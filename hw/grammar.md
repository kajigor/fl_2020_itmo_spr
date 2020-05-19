Пробельные символы и табы, символы переноса строк  никакого смысла не несут.
Они допустимы везде кроме Terminals, NotTerminals, StartSymbol -- там фигурные скобки должны быть "приклеены"
к '#' или к строке терминалов (нетерминалов). Т. е {abcd} -- Terminal, {XYZ} -- NotTerminal


Grammar :: {StartSymbol}, {Terminals}, {NotTerminals}, {Rules}

StartSymbol :: {'#'}

Terminals :: Terminal | Terminal, Terminals

Terminal :: {StringTerm}

NotTerminals :: NotTerminal | NotTerminal, NotTerminals

NotTerminal :: {StringNotTerm}

StringTerm :: LetterSmall  | terminals LetterSmall 

StringNotTerm ::  LetterBig  | StartSymbol |  terminals LetterBig | terminals StartSymbol

LetterSmall :: 'a' | ... | 'z' 

LetterBig :: 'A' | ... | 'Z'

Rules :: StartRule | Rules, Rule

StartRule :: StartSymbol '@' TermsChoice

Rule :: NotTerminal '@' TermsChoice 

RightTerms :: Terminal | NotTerminal | RightTerms Terminal | RightTerms NotTerminal

TermsChoice :: RightTerms | RightTerms '|' TermsChoice
