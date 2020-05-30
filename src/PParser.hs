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
	| HappyAbsSyn7 (Head)
	| HappyAbsSyn8 ([Arg])
	| HappyAbsSyn9 (Arg)
	| HappyAbsSyn10 (Atom)
	| HappyAbsSyn11 (Body)
	| HappyAbsSyn12 ([Atom])
	| HappyAbsSyn13 (Task)

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
 action_25,
 action_26,
 action_27,
 action_28,
 action_29 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_16,
 happyReduce_17 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (PLex.Token)
	-> HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PLex.Token) (HappyStk HappyAbsSyn -> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PLex.Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,46) ([0,0,0,2560,0,0,0,16384,0,128,128,0,0,128,16384,0,32,96,0,1,2,2048,0,0,1152,0,0,0,0,6144,0,0,128,0,0,1152,1536,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","Relations","Rltn","Head","Args","Arg","Atom","Body","Atoms","Task","ident","var","'?-'","':-'","','","'.'","'('","')'","%eof"]
        bit_start = st * 22
        bit_end = (st + 1) * 22
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..21]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (14) = happyShift action_6
action_2 (16) = happyShift action_7
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (22) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 (17) = happyShift action_14
action_5 (11) = happyGoto action_13
action_5 _ = happyReduce_13

action_6 (20) = happyShift action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (14) = happyShift action_11
action_7 (10) = happyGoto action_8
action_7 (12) = happyGoto action_9
action_7 (13) = happyGoto action_10
action_7 _ = happyReduce_17

action_8 _ = happyReduce_15

action_9 (18) = happyShift action_23
action_9 _ = happyReduce_16

action_10 (19) = happyShift action_22
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (20) = happyShift action_21
action_11 _ = happyReduce_11

action_12 (14) = happyShift action_11
action_12 (15) = happyShift action_20
action_12 (8) = happyGoto action_17
action_12 (9) = happyGoto action_18
action_12 (10) = happyGoto action_19
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (19) = happyShift action_16
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (14) = happyShift action_11
action_14 (10) = happyGoto action_8
action_14 (12) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_23
action_15 _ = happyReduce_12

action_16 _ = happyReduce_4

action_17 (18) = happyShift action_26
action_17 (21) = happyShift action_27
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_7

action_19 _ = happyReduce_9

action_20 _ = happyReduce_8

action_21 (14) = happyShift action_11
action_21 (15) = happyShift action_20
action_21 (8) = happyGoto action_25
action_21 (9) = happyGoto action_18
action_21 (10) = happyGoto action_19
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_1

action_23 (14) = happyShift action_11
action_23 (10) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_14

action_25 (18) = happyShift action_26
action_25 (21) = happyShift action_29
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_11
action_26 (15) = happyShift action_20
action_26 (9) = happyGoto action_28
action_26 (10) = happyGoto action_19
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_5

action_28 _ = happyReduce_6

action_29 _ = happyReduce_10

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
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
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Line happy_var_1 happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (PLex.TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Head happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyTerminal (PLex.TVar happy_var_1))
	 =  HappyAbsSyn9
		 (Left happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Right happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 10 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (PLex.TId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Atom happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal (PLex.TId happy_var_1))
	 =  HappyAbsSyn10
		 (Atom happy_var_1 []
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Body (reverse happy_var_2)
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  11 happyReduction_13
happyReduction_13  =  HappyAbsSyn11
		 (Body  []
	)

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (Task (reverse happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  13 happyReduction_17
happyReduction_17  =  HappyAbsSyn13
		 (Task []
	)

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PLex.TId happy_dollar_dollar -> cont 14;
	PLex.TVar happy_dollar_dollar -> cont 15;
	PLex.TTrg -> cont 16;
	PLex.TNeck -> cont 17;
	PLex.TCom -> cont 18;
	PLex.TPrd -> cont 19;
	PLex.TLbr -> cont 20;
	PLex.TRbr -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
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

data P = P [Line] Task

instance Show P where
    show (P lines task) = printf "%s ?- %s." (intercalate "\n" (map show lines)) (show task)

data Line = Line Head Body

instance Show Line where
    show (Line head body@(Body b)) | null b = printf "%s." (show head)
                                   | otherwise = printf "%s :- %s." (show head) (show body)


data Head = Head Ident [Arg]

instance Show Head where
    show (Head id args) | null args = printf "%s" id
                        | otherwise = printf "%s(%s)" id (commaFold (map show args))

type Ident = String 

type Var = String 

type Arg = Either Var Atom

data Body = Body [Atom]

instance Show Body where
    show x = case x of
        Body b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

data Atom = Atom Ident [Arg]

instance Show Atom where
    show x = case x of
        Atom id args -> if null args
                        then printf "%s" id
                        else printf "%s(%s)" id (commaFold (map show args))

data Task = Task [Atom]
instance Show Task where
    show x = case x of
        Task b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

parseError = undefined

runParser = print . parse . PLex.alexScanTokens
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
