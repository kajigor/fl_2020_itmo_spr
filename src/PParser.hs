{-# OPTIONS_GHC -w #-}
module PParser where

import Data.List (intercalate)
import qualified PLex
import Text.Printf
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (PLex.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (P)
	| HappyAbsSyn5 ([Line])
	| HappyAbsSyn6 (Line)
	| HappyAbsSyn7 ([Arg])
	| HappyAbsSyn8 (Arg)
	| HappyAbsSyn9 (Atom)
	| HappyAbsSyn10 (Body)
	| HappyAbsSyn11 ([Atom])
	| HappyAbsSyn12 (Task)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (PLex.Token)
	-> HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> m HappyAbsSyn)
	-> [HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PLex.Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (PLex.Token)
	-> HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (PLex.Token)
	-> HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,39) ([0,0,0,320,0,0,0,256,0,32769,0,0,8192,0,8,24,8192,8192,0,64,0,0,9,0,0,0,0,0,32,0,32768,1,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","Relations","Rltn","Args","Arg","Atom","Body","Atoms","Task","ident","var","'?-'","':-'","','","'.'","'('","')'","%eof"]
        bit_start = st * 21
        bit_end = (st + 1) * 21
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..20]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (13) = happyShift action_6
action_2 (15) = happyShift action_7
action_2 (6) = happyGoto action_4
action_2 (9) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (21) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (16) = happyShift action_13
action_5 (10) = happyGoto action_12
action_5 _ = happyReduce_12

action_6 (19) = happyShift action_11
action_6 _ = happyReduce_10

action_7 (13) = happyShift action_6
action_7 (9) = happyGoto action_8
action_7 (11) = happyGoto action_9
action_7 (12) = happyGoto action_10
action_7 _ = happyReduce_16

action_8 _ = happyReduce_14

action_9 (17) = happyShift action_21
action_9 _ = happyReduce_15

action_10 (18) = happyShift action_20
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (13) = happyShift action_6
action_11 (14) = happyShift action_19
action_11 (7) = happyGoto action_16
action_11 (8) = happyGoto action_17
action_11 (9) = happyGoto action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (18) = happyShift action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (13) = happyShift action_6
action_13 (9) = happyGoto action_8
action_13 (11) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (17) = happyShift action_21
action_14 _ = happyReduce_11

action_15 _ = happyReduce_4

action_16 (17) = happyShift action_23
action_16 (20) = happyShift action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_6

action_18 _ = happyReduce_8

action_19 _ = happyReduce_7

action_20 _ = happyReduce_1

action_21 (13) = happyShift action_6
action_21 (9) = happyGoto action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_13

action_23 (13) = happyShift action_6
action_23 (14) = happyShift action_19
action_23 (8) = happyGoto action_25
action_23 (9) = happyGoto action_18
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_9

action_25 _ = happyReduce_5

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (P (reverse happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (Line happy_var_1 happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyTerminal (PLex.TVar happy_var_1))
	 =  HappyAbsSyn8
		 (V happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (A happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (PLex.TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Atom happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyTerminal (PLex.TId happy_var_1))
	 =  HappyAbsSyn9
		 (Atom happy_var_1 []
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Body (reverse happy_var_2)
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10 happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 (Body  []
	)

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Task (reverse happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  12 happyReduction_16
happyReduction_16  =  HappyAbsSyn12
		 (Task []
	)

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PLex.TId happy_dollar_dollar -> cont 13;
	PLex.TVar happy_dollar_dollar -> cont 14;
	PLex.TTrg -> cont 15;
	PLex.TNeck -> cont 16;
	PLex.TCom -> cont 17;
	PLex.TPrd -> cont 18;
	PLex.TLbr -> cont 19;
	PLex.TRbr -> cont 20;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 21 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(PLex.Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


commaFold :: [String] -> String
commaFold xs = intercalate ", " xs

data P = P [Line] Task deriving (Eq)

instance Show P where
    show (P lines task) = printf "%s ?- %s." (intercalate "\n" (map show lines)) (show task)

data Line = Line Atom Body deriving (Eq)

instance Show Line where
    show (Line head body@(Body b)) | null b = printf "%s." (show head)
                                   | otherwise = printf "%s :- %s." (show head) (show body)

type Ident = String 

type Var = String 

{- type Arg = Either Var Atom -}

data Helper a b = V a | A b deriving (Eq)
type Arg = Helper Var Atom

instance (Show a, Show b) => Show (Helper a b) where
    show x = case x of
        V a -> show a
        A b -> show b

data Body = Body [Atom] deriving (Eq)

instance Show Body where
    show x = case x of
        Body b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

data Atom = Atom Ident [Arg] deriving (Eq)

instance Show Atom where
    show x = case x of
        Atom id args -> if null args
                        then printf "%s" id
                        else printf "%s(%s)" id (commaFold (map show args))

data Task = Task [Atom] deriving (Eq)
instance Show Task where
    show x = case x of
        Task b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

parseError :: [PLex.Token] -> a
parseError ts = error ("parse error on token" ++ (show ts))

runParser = print . parse . PLex.alexScanTokens

orig = 
    [ "eval(St, var(X), U) :- elem(X, St, U)."
    , "eval(St, conj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), and(V, W, U)."
    , "eval(St, disj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), or(V, W, U)."
    , "eval(St, not(X), U) :- eval(St, X, V), neg(U, V)."
    , "elem(zero, cons(H,T), H)."
    , "elem(succ(N), cons(H,T), V) :- elem(N, T, V)."
    , "nand(false, false, true)."
    , "nand(false, true, true)."
    , "nand(true, false, true)."
    , "nand(true, true, false)."
    , "neg(X, R) :- nand(X, X, R)."
    , "or(X, Y, R) :- nand(X, X, Xx), nand(Y, Y, Yy), nand(Xx, Yy, R)."
    , "and(X, Y, R) :- nand(X, Y, Xy), nand(Xy, Xy, R)."
    , "?- eval(St, conj(disj(X,Y),not(var(Z))), true)."
    ]
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
