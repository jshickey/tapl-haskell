module Main where

import Control.Monad
import Test.HUnit

import ArithTests
import qualified FullSimpleTests as F
import qualified SubtypeTests as ST
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

-- A lot of the tests are inherited from FullSimpleTests.  The ones 
-- specifically for references are listed below.

fullRefParseTests 
    = [ ("ref", TmRef (TmSucc TmZero), "ref 1;")
      , ("deref", TmDeref (TmRef TmZero), "! (ref 0);")
      ]

fullRefEvalTests 
    = [ ("ref", "x : Ref Nat", "x = ref 5;")
      , ("deref ref", "4 : Nat", "! (ref 4);")
      , ("deref", "x : Ref Nat\n5 : Nat", "x = ref 5; !x;")
      , ("multiple refs", "x : Ref Nat\ny : Ref Nat\n5 : Nat\n8 : Nat",
         "x = ref 5; y = ref 8; !x; !y;")
      , ("change ref", "x : Ref Nat\nunit : Unit\n8 : Nat\n",
         "x = ref 5; x := 8; !x;")
      , ("shared state", "x : Ref Nat\ny : Ref Nat\nunit : Unit\n6 : Nat",
         "x = ref 5; y = x; x := 6; !y;")
      , ("ref subtype", "true : Bool",
         "(lambda r:Ref {x:Bool,y:Nat}. (!r).x) (ref {y=0,x=true});")
      , ("join ref 1", "{x=true, y=3} : {y:Nat, x:Bool}",
         "!(if true then ref {x=true, y=3} else ref {y = 4, x=false});")
      ]

fullRefEvalErrorTests
    = [ ("ref bad subtype 1", show badApplication,
         "(lambda r:Ref {x:Bool,y:Nat,z:Nat}. (!r).x) (ref {y=0,x=true});")
      , ("ref bad subtype 2", show badApplication,
         "(lambda r:Ref {x:Bool}. (!r).x) (ref {y=0,x=true});")
      ] 

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullRef)    F.parseTests
                        , map (makeEvalTest  parseAndEval)    F.evalTests
                        , map (makeParseTest parseFullRef)    fullRefParseTests
                        , map (makeEvalTest  parseAndEval)    fullRefEvalTests
                        , map (makeEvalTest  parseAndEval)    fullRefEvalErrorTests
                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests
                        , map (makeEvalTest  parseAndEval)    ST.fullsubEvalTests
                        , map (makeEvalTest  parseAndEval)    ST.fullsubEvalErrorTests                       
                        , [testDotFTest]
                        ]
                         
main :: IO ()
main = getAllTests >>= runTests
