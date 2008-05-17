module Main where

import Control.Monad
import HUnit

import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

parseTests = [("comments", TmTrue, "/**** comment *****/true /* another*//**/;")
             ,("true",  TmTrue,  "true;")
             ,("false", TmFalse, "false;")
             ,("0",     TmZero,  "0;")
             ,("VarBind to type", TmBind "x" (VarBind TyBool), "x : Bool;")
             ]

evalTests = [("true",  "true : Bool",  "true;")
            ,("false", "false : Bool", "false;")
            ,("0",     "0 : Nat",      "0;")
            ,("varbind to type", "x : Bool\nx : Bool", "x : Bool; x;")
            ,("multiple varbinds to type",
              "x : Bool\nx : Bool\ny : Nat\nx : Bool\ny : Nat",
              "x : Bool; x;y : Nat; x; y;")
            ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) parseTests
                        , map (makeEvalTest  parseAndEval)    evalTests
-- TODO                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
