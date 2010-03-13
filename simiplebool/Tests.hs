module Main where

import Control.Monad
import Test.HUnit
import Evaluator
import TaplError
import SimpleBool
import Parser
import TestUtils

subTests = [("no sub var",  TmVar 0 1, 1, 
                 TmVar 5 7, TmVar 0 1)
           ,("yes sub var", TmVar 5 7, 1, 
                 TmVar 5 7, TmVar 1 1)
           ,("no sub in abs",  TmAbs "x" TyBool (TmVar 0 1), 0, 
                   TmVar 5 7,  TmAbs "x" TyBool (TmVar 0 1))
           ,("yes sub in abs", TmAbs "x" TyBool (TmVar 6 8), 0, 
                    TmVar 5 7, TmAbs "x" TyBool (TmVar 1 1))
           ,("no sub in app",  TmApp (TmVar 0 1) (TmVar 2 3), 1,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("one sub in app", TmApp (TmVar 5 7) (TmVar 2 3), 0,
                   TmVar 5 7,  TmApp (TmVar 0 1) (TmVar 2 3))
           ,("two sub in app", TmApp (TmVar 5 7) (TmVar 5 7), 2,
                   TmVar 5 7,  TmApp (TmVar 2 1) (TmVar 2 3))
           ,("sub true", TmTrue, 0, TmVar 0 1, TmTrue)
           ,("sub false", TmFalse, 0, TmVar 0 1, TmFalse)
           ,("sub if 1", TmIf (TmVar 5 7) (TmVar 0 1) (TmVar 5 7), 1,
             TmVar 5 7,  TmIf (TmVar 1 1) (TmVar 0 1) (TmVar 1 1))
           ,("sub if 2", TmIf (TmVar 2 1) (TmVar 5 7) (TmVar 2 1), 1,
             TmVar 5 7,  TmIf (TmVar 2 1) (TmVar 1 5) (TmVar 2 1))
           ]

makeSubTest (label, expected, idx, replacement, term)
    = TestLabel label (TestCase (assertEqual "Incorrect substitution"
                                             expected
                                             (sub idx replacement term)))

shiftTests = [("no shift var",  TmAbs "x" TyBool (TmVar 0 3), 
                            2,  TmAbs "x" TyBool (TmVar 0 1))
             ,("yes shift var", TmAbs "x" TyBool (TmVar 3 6), 
                            2,  TmAbs "x" TyBool (TmVar 1 4))
             ,("no shift two abs",   TmAbs "x" TyBool (TmAbs "y" TyBool (TmVar 1 5)), 
                                4,   TmAbs "x" TyBool (TmAbs "y" TyBool (TmVar 1 1)))
             ,("yes shift two abs",  TmAbs "x" TyBool (TmAbs "y" TyBool (TmVar 6 5)), 
                                4,   TmAbs "x" TyBool (TmAbs "y" TyBool (TmVar 2 1)))
             ,("no shift in app", 
                  TmApp (TmAbs "x" TyBool (TmVar 0 4)) (TmAbs "x" TyBool (TmVar 0 2)),
               1, TmApp (TmAbs "x" TyBool (TmVar 0 3)) (TmAbs "x" TyBool (TmVar 0 1)))
             ,("one shift in app", 
                  TmApp (TmAbs "x" TyBool (TmVar 3 4)) (TmAbs "x" TyBool (TmVar 0 2)),
               1, TmApp (TmAbs "x" TyBool (TmVar 2 3)) (TmAbs "x" TyBool (TmVar 0 1)))
             ,("two shifts in app", 
                  TmApp (TmAbs "x" TyBool (TmVar 3 4)) (TmAbs "x" TyBool (TmVar 2 2)),
               1, TmApp (TmAbs "x" TyBool (TmVar 2 3)) (TmAbs "x" TyBool (TmVar 1 1)))
             ,("shift true", TmTrue, 1, TmTrue)
             ,("shift false", TmFalse, 1, TmFalse)
             ,("shift if 1", 
               TmAbs "x" TyBool (TmIf (TmVar 2 2) (TmVar 0 2) (TmVar 2 2)), 1,
               TmAbs "x" TyBool (TmIf (TmVar 1 1) (TmVar 0 1) (TmVar 1 1)))
             ,("shift if 2", 
               TmAbs "x" TyBool (TmIf (TmVar 0 4) (TmVar 6 8) (TmVar 0 4)), 3,
               TmAbs "x" TyBool (TmIf (TmVar 0 1) (TmVar 3 5) (TmVar 0 1)))
             ]

makeShiftTest (label, expected, inc, term)
    = TestLabel label (TestCase (assertEqual "Incorrect shift"
                                             expected
                                             (shift inc term)))

simpleBoolParseTests 
    = [("whitespace1", TmTrue, "  \ttrue;"),
       ("whitespace2", TmTrue, "true;   "),
       ("whitespace2", TmFalse, "false  ; "),
       ("comments1", TmTrue, "/* comment */true;"),
       ("comments2", TmTrue, "/* comment */ true/***** foo */  ; "),
       ("comments3", TmTrue, "/* comment ******/true;"),
       ("bind", TmBind "x" (VarBind TyBool), "x : Bool;"),
       ("abs + app", TmAbs "x" TyBool (TmApp (TmVar 0 1) (TmVar 0 1)), 
        "lambda x : Bool. x x;"),
       ("app associativity", 
        TmAbs "x" TyBool (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                             (TmVar 0 1)),
        "lambda x : Bool. x x x;"),
       ("true", TmTrue, "true;"),
       ("true + parens", TmTrue, "(true);"),
       ("false", TmFalse, "false;"),
       ("false + parens", TmFalse, "(false);"),
       ("if", TmIf TmTrue TmTrue TmFalse, "if true then true else false;"),
       ("if + parens", TmIf TmTrue TmTrue TmFalse, "(if true then true else false);")
      ]

simpleBoolEvalTests 
    = [("true", "true : Bool", "true;")
      ,("false", "false : Bool", "false;")
      ,("E-IfTrue", "true : Bool", "if true then true else false;")
      ,("E-IfFalse", "false : Bool", "if false then true else false;")
      ,("E-If", "true : Bool", "if if false then false else true then true else false;")
      ,("binder", "x : Bool", "x : Bool;")
      ,("binder + usage", "x : Bool\nx : Bool", "x : Bool;x;")
      ,("multi-binder", "x : Bool\nx : Bool\ny : Bool\ny : Bool\nx : Bool", "x:Bool;x;y : Bool;y;x;")
      ,("lambda", "(lambda x:Bool -> Bool. x) : (Bool -> Bool) -> Bool -> Bool", 
                    "lambda x:Bool->Bool. x;")
      ,("lambda + app", "true : Bool", "(lambda x:Bool->Bool. x) true;")
      ,("binder + lambda", "x : Bool\n(lambda x':Bool. x') : Bool -> Bool", 
                             "x : Bool; lambda x : Bool. x;")
      ,("lambda of lamdba", "(lambda x:Bool -> Bool. x) : (Bool -> Bool) -> Bool -> Bool", "(lambda x : Bool->Bool. x);")
      ,("apply lambda", "(lambda y:Bool. y) : Bool -> Bool", 
         "(lambda x : Bool->Bool. x) (lambda y:Bool. y);")
      ,("multiple application", "false : Bool", 
        "(lambda x : Bool->Bool. x) (lambda y:Bool. y) false;")
      ];

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map makeShiftTest shiftTests,
                                             map makeSubTest subTests,
                                             map (makeParseTest parseSimpleBool) simpleBoolParseTests,
                                             map (makeEvalTest parseAndEval) simpleBoolEvalTests,
                                             [testDotFTest]
                                            ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests
