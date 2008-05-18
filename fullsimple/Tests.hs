module Main where

import Control.Monad
import HUnit

import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

-- FORMAT: (test name, expected parse tree, input)
parseTests = [("comments", TmTrue, "/**** comment *****/true /* another*//**/;")
             ,("true",  TmTrue,  "true;")
             ,("false", TmFalse, "false;")
             ,("0",     TmZero,  "0;")
             ,("VarBind to type", TmBind "x" (VarBind TyBool), "x : Bool;")
             ,("lambda", TmAbs "x" TyBool (TmVar 0 1), "(lambda x:Bool. x);")
             ,("nested lambda 1", 
               TmAbs "x" TyBool (TmAbs "y" TyNat (TmVar 0 2)),
               "(lambda x:Bool. (lambda y:Nat. y));")
             ,("nested lambda 2", 
               TmAbs "x" TyBool (TmAbs "y" TyNat (TmVar 1 2)),
               "(lambda x:Bool. (lambda y:Nat. x));")
             ,("application", TmApp (TmAbs "x" TyBool (TmVar 0 1)) TmTrue, 
               "(lambda x:Bool. x) true;")
             ,("app left-assoc", TmApp (TmApp TmTrue TmZero) TmFalse,
               "true 0 false;")
             ]

-- FORMAT: (test name, expected printed output, input)
evalTests = [("true",  "true : Bool",  "true;")
            ,("false", "false : Bool", "false;")
            ,("0",     "0 : Nat",      "0;")
            ,("varbind to type", "x : Bool\nx : Bool", "x : Bool; x;")
            ,("multiple varbinds to type",
              "x : Bool\nx : Bool\ny : Nat\nx : Bool\ny : Nat",
              "x : Bool; x;y : Nat; x; y;")
            ,("lambda", 
              "(lambda x:Bool. x) : Bool -> Bool",
              "(lambda x:Bool. x);")
            ,("lambda + freshname", 
              "x : Bool\n(lambda x':Bool. x') : Bool -> Bool",
              "x : Bool; (lambda x:Bool. x);")
            ,("application", 
              "true : Bool",
              "(lambda x:Bool. x) true;")
            ,("nested lambda 1", 
              "(lambda x:Bool. (lambda y:Nat. y)) : Bool -> Nat -> Nat",
              "(lambda x:Bool. (lambda y:Nat. y));")
            ,("apply nested lambda 1", 
              "0 : Nat",
              "(lambda x:Bool. (lambda y:Nat. y)) true 0;")
            ,("nested lambda 2", 
              "(lambda x:Bool. (lambda y:Nat. x)) : Bool -> Nat -> Bool",
              "(lambda x:Bool. (lambda y:Nat. x));")
            ,("apply nested lambda 2", 
              "true : Bool",
              "(lambda x:Bool. (lambda y:Nat. x)) true 0;")
            ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) parseTests
                        , map (makeEvalTest  parseAndEval)    evalTests
-- TODO                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
