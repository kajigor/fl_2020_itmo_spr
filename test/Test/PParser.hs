module Test.PParser where

import           PParser                        ( parse
                                                , P(..)
                                                , Task(..)
                                                , Line(..)
                                                , Head(..)
                                                , Body(..)
                                                , Helper(..)
                                                , Atom(..)
                                                )
import           PLex                           ( alexScanTokens )

import           Test.Tasty.HUnit               ( Assertion
                                                , assertBool
                                                , (@?=)
                                                )

orig :: [String]
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

unit_parser :: Assertion
unit_parser = do
    parse (alexScanTokens "?-.") @?= P [] (Task [])
    parse (alexScanTokens (head orig ++ last orig)) @?= P
        [ Line (Head "eval" [V "St", A (Atom "var" [V "X"]), V "U"])
               (Body [Atom "elem" [V "X", V "St", V "U"]])
        ]
        (Task
            [ Atom
                  "eval"
                  [ V "St"
                  , A
                      (Atom
                          "conj"
                          [ A (Atom "disj" [V "X", V "Y"])
                          , A (Atom "not" [A (Atom "var" [V "Z"])])
                          ]
                      )
                  , A (Atom "true" [])
                  ]
            ]
        )



