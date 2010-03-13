module Main where

import Control.Monad
import Test.HUnit

import ArithTests
import qualified FullSimpleTests as F
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

-- A lot of the tests are inherited from FullSimpleTests.  The ones 
-- specifically for references are listed below.

fullErrorParseTests 
    = [ ("error", TmError TyBot, "error;")
      , ("try/with", TmTry TmTrue TmFalse, "try true with false;")
      ]

fullErrorEvalTests 
    = [ ("error", "error : Bot", 
         "error;")
      , ("error result", "error : Bot", 
         "(lambda y: Bool. error) true;")
      , ("propagate error", "error : Bot", 
         "(lambda x:Bool. (lambda y:Bool. error) x) true;")
      , ("try - fail", "true : Bool", 
         "try error with true;")
      , ("try - success with eval", "true : Bool", 
         "try ((lambda x:Bool. x) true) with ((lambda x:Bool. x) false);")
      , ("try - fail with eval", "false : Bool", 
         "try ((lambda x:Bool. x) error) with false;")
      , ("if error", "error : Bool", "if error then true else false;")
      , ("succ error", "error : Nat", "succ error;")
      , ("pred error", "error : Nat", "pred pred error;")
      , ("float error 1", "error : Float", "timesfloat error 1.2;")
      , ("float error 2", "error : Float", "timesfloat 1.1 error;")
      ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullError) F.parseTests
                        , map (makeEvalTest  parseAndEval)   F.evalTests
                        , map (makeParseTest parseFullError) fullErrorParseTests
                        , map (makeEvalTest  parseAndEval)   fullErrorEvalTests
                        , map (makeEvalTest  parseAndEval)   tyarithEvalTests
                        , [testDotFTest]
                        ]
                         
main :: IO ()
main = getAllTests >>= runTests
