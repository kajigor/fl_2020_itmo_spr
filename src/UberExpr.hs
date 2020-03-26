module UberExpr where

import           Combinators (Parser (..))
import           Control.Applicative(Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> (op -> ast -> ast)

         -> Parser e i ast
uberExpr [] elemParser _ _ = elemParser
uberExpr ((opParser, optype):ps) elemParser binOpFunc unOpFunc = 
  let elemParser' = uberExpr ps elemParser binOpFunc unOpFunc
      rightParser = ((\x f y -> binOpFunc f x y) <$> elemParser' <*> opParser <*> rightParser) <|> elemParser'
      noAssocParser = ((\x f y -> binOpFunc f x y) <$> elemParser' <*> opParser <*> elemParser') <|> elemParser'
      leftParser = elemParser' >>= continueLeftParsing
      continueLeftParsing left = (do
        op <- opParser
        right <- elemParser'
        continueLeftParsing $ binOpFunc op left right) <|> return left
      unaryParser = ((\f x -> unOpFunc f x) <$> opParser <*> elemParser') <|> elemParser'
  in case optype of
    Unary -> unaryParser
    (Binary assoc) -> case assoc of
      LeftAssoc -> leftParser
      RightAssoc -> rightParser
      NoAssoc -> noAssocParser
