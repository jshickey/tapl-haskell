module Main where

import Control.Monad
import Test.HUnit
import Evaluator
import TaplError
import Untyped
import UntypedParser
import UntypedTests
import TestUtils

subTests = [("no sub var",  TmVar 0 1, 1, 
                 TmVar 5 7, TmVar 0 1)
           ,("yes sub var", TmVar 5 7, 1, 
                 TmVar 5 7, TmVar 1 1)
           ,("no sub in abs",  TmAbs "x" (TmVar 0 1), 0, 
                   TmVar 5 7,  TmAbs "x" (TmVar 0 1))
           ,("yes sub in abs", TmAbs "x" (TmVar 6 8), 0, 
                    TmVar 5 7, TmAbs "x" (TmVar 1 1))
           ,("no sub in app",  TmApp (TmVar 0 1) (TmVar 2 3), 1,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("one sub in app", TmApp (TmVar 5 7) (TmVar 2 3), 0,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("two sub in app", TmApp (TmVar 5 7) (TmVar 5 7), 2,
                   TmVar 5 7,  TmApp (TmVar 2 1) (TmVar 2 3))
           ]

makeSubTest (label, expected, idx, replacement, term)
    = TestLabel label (TestCase (assertEqual "Incorrect substitution"
                                             expected
                                             (sub idx replacement term)))

shiftTests = [("no shift var",  TmAbs "x" (TmVar 0 3), 
                            2,  TmAbs "x" (TmVar 0 1))
             ,("yes shift var", TmAbs "x" (TmVar 3 6), 
                            2,  TmAbs "x" (TmVar 1 4))
             ,("no shift two abs",   TmAbs "x" (TmAbs "y" (TmVar 1 5)), 
                                4,   TmAbs "x" (TmAbs "y" (TmVar 1 1)))
             ,("yes shift two abs",  TmAbs "x" (TmAbs "y" (TmVar 6 5)), 
                                4,   TmAbs "x" (TmAbs "y" (TmVar 2 1)))
             ,("no shift in app", 
                  TmApp (TmAbs "x" (TmVar 0 4)) (TmAbs "x" (TmVar 0 2)),
               1, TmApp (TmAbs "x" (TmVar 0 3)) (TmAbs "x" (TmVar 0 1)))
             ,("one shift in app", 
                  TmApp (TmAbs "x" (TmVar 3 4)) (TmAbs "x" (TmVar 0 2)),
               1, TmApp (TmAbs "x" (TmVar 2 3)) (TmAbs "x" (TmVar 0 1)))
             ,("two shifts in app", 
                  TmApp (TmAbs "x" (TmVar 3 4)) (TmAbs "x" (TmVar 2 2)),
               1, TmApp (TmAbs "x" (TmVar 2 3)) (TmAbs "x" (TmVar 1 1)))
             ]

makeShiftTest (label, expected, inc, term)
    = TestLabel label (TestCase (assertEqual "Incorrect shift"
                                             expected
                                             (shift inc term)))

untypedParseTests = [("whitespace1", TmBind "x", "  \tx/;"),
              ("whitespace2", TmBind "x", "x/;   "),
              ("whitespace2", TmBind "x", "x / ; "),
              ("comments1", TmBind "x", "/* comment */x/;"),
              ("comments2", TmBind "x", "/* comment */ x/***** foo */  /; "),
              ("comments3", TmBind "x", "/* comment ******/x/;"),
              ("bind", TmBind "x", "x/;"),
              ("abs + app", TmAbs "x" (TmApp (TmVar 0 1) (TmVar 0 1)), 
                              "lambda x. x x;"),
              ("app associativity", 
               TmAbs "x" (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                                (TmVar 0 1)),
               "lambda x. x x x;")
             ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map makeShiftTest shiftTests,
                                             map makeSubTest subTests,
                                             map (makeParseTest parseUntyped) untypedParseTests,
                                             map (makeEvalTest parseAndEval) untypedEvalTests,
                                             [testDotFTest]
                                            ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests
