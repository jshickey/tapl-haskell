module Main where

import Control.Monad
import HUnit
import TaplError
import Typing
import TestUtils

tyarithEvalTests
    = [("true", "true : Bool", "true;"),
       ("false", "false : Bool", "false;"),
       ("zero", "0 : Nat", "0;"),
       ("E-IfTrue", "0 : Nat", "if true then 0 else false;"),
       ("E-IfFalse", "false : Bool", "if false then 0 else false;"),
       ("E-If", "0 : Nat", "if if false then false else true then 0 else false;"),
       ("succ", "1 : Nat", "succ 0;"),
       ("E-Succ", "1 : Nat", "succ if true then 0 else false;"),
       ("E-PredSucc", "0 : Nat", "pred (succ 0);"),
       ("E-PredZero", "0 : Nat", "pred 0;"),
       ("E-Pred", "0 : Nat", "pred (pred 0);"),
       ("test.f #4", "1 : Nat", "succ (pred 0);"),
       ("test.f #5", "false : Bool", "iszero (pred (succ (succ 0)));"),
       ("E-IsZeroZero", "true : Bool", "iszero 0;"),
       ("E-IsZeroSucc", "false : Bool", "iszero (succ 0);"),
       ("E-IsZero", "true : Bool", "iszero (pred (succ 0));")
      ]

tyarithTypeErrorTests
    = [("predMismatch", show predMismatch, "if 0 then true else false;")
-- It's not possible to get a cond-alt type mismatch using the arith 
-- evaluator, because it will either evaluate to one of them, or else
-- fail earlier with a predMismatch
--      ,("condAltMismatch", show condAltMismatch, "if true then 0 else false;")
      ,("expectedNat 1", show expectedNat, "succ true;")
      ,("expectedNat 2", show expectedNat, "pred false;")
      ,("expectedNat 3", show expectedNat, "iszero false;")
      ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map (makeEvalTest parseAndEval) tyarithEvalTests,
                                             map (makeEvalTest parseAndEval) tyarithTypeErrorTests,
                                             [testDotFTest]
                                            ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests


