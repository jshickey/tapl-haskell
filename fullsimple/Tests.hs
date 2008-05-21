module Main where

import Control.Monad
import HUnit

import ArithTests
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
             ,("if", TmIf TmTrue TmZero TmFalse, 
               "if true then 0 else false;")
             ,("if + parens", TmIf TmTrue TmZero TmFalse, 
               "(if true then 0 else false);")
             ,("succ", TmSucc TmZero, "succ 0;")
             ,("pred", TmPred TmZero, "pred 0;")
             ,("iszero", TmIsZero TmZero, "iszero 0;")
             ,("unit", TmUnit, "unit;")
             ,("string", TmString "foo", "\"foo\";")
             ,("float", TmFloat 1.2, "1.2;")
             ,("timesfloat", TmTimesFloat (TmFloat 1.1) (TmFloat 1.2), 
               "timesfloat 1.1 1.2;")
             ,("wildcard", TmAbs "_" TyBool TmTrue, "lambda _:Bool. true;")
             ,("TyId", TmAbs "x" (TyId "A") TmTrue, "lambda x:A. true;")
             ,("TyAbbBind", TmBind "A" (TyAbbBind TyBool), "A = Bool;")
             ,("TyVarBind", TmBind "A" TyVarBind, "A;")
             ,("TmAbbBind", TmBind "x" (TmAbbBind TmZero (Just TyNat)), "x=0;")
             ,("let x", TmLet "x" TmTrue (TmVar 0 1), "let x = true in x;")
             ,("let _", TmLet "_" TmTrue TmFalse, "let _ = true in false;")
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
            ,("nested lambda 3", 
              "(lambda f:Nat -> Nat. (lambda x:Nat. f (f x))) : (Nat -> Nat) -> Nat -> Nat",
              "lambda f:Nat->Nat. lambda x:Nat. f (f x);")
            ,("apply nested lambda 2", 
              "true : Bool",
              "(lambda x:Bool. (lambda y:Nat. x)) true 0;")
            ,("unit", "unit : Unit", "unit;")
            ,("string", "\"foo\" : String", "\"foo\";")
            ,("float", "1.2 : Float", "1.2;")
            ,("timesfloat", "1.32 : Float", 
               "timesfloat 1.1 1.2;")
            ,("times float after eval", "1.32 : Float", 
              "timesfloat 1.1 ((lambda x:Float. 1.2) 3.0);")
            ,("wildcard", "(lambda _:Bool. true) : Bool -> Bool", 
              "lambda _:Bool. true;")
            ,("apply wildcard", "true : Bool", "(lambda _:Bool. true) false;")
            ,("TyVarBind", "A", "A;")
            ,("TyId", "(lambda x:A. x) : A -> A", "lambda x:A. x;")
            ,("TyAbbBind", "Bfun :: *", "Bfun = Bool -> Bool;")
            ,("use of TyAbbBind", 
              "T :: *\n(lambda f:T. (lambda x:Nat. f (f x))) : T -> Nat -> Nat",
             "T = Nat->Nat; lambda f:T. lambda x:Nat. f (f x);")
            ,("TmAbbBind", "x : Nat\n0 : Nat", "x = 0; x;")
            ,("let x", "true : Bool", "let x = true in x;")
            ,("let _", "false : Bool", "let _ = true in false;")
            ,("nested lets", "1.32 : Float", 
              "let x = 1.2 in let y = 1.1 in timesfloat y x;")
            ,("eval let term", "1.32 : Float",
              "let x = (timesfloat 1.1 1.2) in x;")
            ,("shift let", "1.3 : Float",
              "(lambda x:Float. let y = x in timesfloat y 1.3) 1.0;")
            ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) parseTests
                        , map (makeEvalTest  parseAndEval)    evalTests
                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests
-- TODO                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
