module Main where

import Control.Monad
import Test.HUnit
import Evaluator
import TaplError
import Arith
import ArithParser
import ArithTests
import TestUtils

arithParseTests 
    = [("true", TmTrue, "true;"),
       ("true + parens", TmTrue, "(true);"),
       ("false", TmFalse, "false;"),
       ("false + parens", TmFalse, "(false);"),
       ("zero", TmZero, "0;"),
       ("zero + parens", TmZero, "(0);"),
       ("if", TmIf TmTrue TmZero TmFalse, "if true then 0 else false;"),
       ("if + parens", TmIf TmTrue TmZero TmFalse, "(if true then 0 else false);"),
       ("succ", TmSucc TmZero, "succ 0;"),
       ("pred", TmPred TmZero, "pred 0;"),
       ("iszero", TmIsZero TmZero, "iszero 0;"),
       ("combo1", TmSucc (TmPred TmZero), "(succ (pred 0));"),
       ("combo2", TmIsZero (TmSucc (TmPred TmZero)), "iszero (succ (pred 0));"),
       ("whitespace1", TmTrue, "  \ttrue;"),
       ("whitespace2", TmTrue, "true;   "),
       ("whitespace2", TmTrue, "true  ; "),
       ("comments1", TmZero, "/* comment */0;"),
       ("comments2", TmZero, "/* comment */ 0/***** foo */  ; "),
       ("comments3", TmZero, "/* comment ******/0;")
      ];

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map (makeParseTest parseArith) arithParseTests,
                                             map (makeEvalTest parseAndEval) arithEvalTests,
                                             [testDotFTest]
                                            ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests


