module UberExpr where

import           Combinators (Parser (..))
import           Control.Applicative(Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] elemParser _ = elemParser
uberExpr ((opParser,assoc):ps) elemParser opFunc = 
  let elemParser' = uberExpr ps elemParser opFunc
      rightParser = ((\x f y -> opFunc f x y) <$> elemParser' <*> opParser <*> rightParser) <|> elemParser'
      noAssocParser = ((\x f y -> opFunc f x y) <$> elemParser' <*> opParser <*> elemParser') <|> elemParser'
      leftParser = elemParser' >>= continueLeftParsing
      continueLeftParsing left = (do
        op <- opParser
        right <- elemParser'
        continueLeftParsing $ opFunc op left right) <|> return left
  in case assoc of
    LeftAssoc -> leftParser
    RightAssoc -> rightParser
    NoAssoc -> noAssocParser
