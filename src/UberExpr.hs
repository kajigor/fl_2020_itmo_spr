module UberExpr where

import           Combinators (Parser (..), runParser)
import           Control.Applicative(Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: (Monoid e, Eq e)
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
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
