module UberExpr where

import           Combinators (Parser (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast
uberExpr [] elemParser _ = elemParser
uberExpr ((opParser,assoc):ps) elemParser opFunc = 
  let elemParser' = uberExpr ps elemParser
      rightParser = ((\x f y -> opFunc f x y) <$> elemParser' <*> opParser <*> rightParser) <|> elemParser'
      noAssocParser = undefined
      leftParser = (do
        left <- elemParser'
        op <- opParser
        right <- elemParser'
        return $ opFunc op left right) <|> elemParser'
  in case assoc of
    LeftAssoc -> undefined
    RightAssoc -> rightParser
    NoAssoc -> undefined

--parseRightAssoc opParser = ((\x f y -> BinOp
