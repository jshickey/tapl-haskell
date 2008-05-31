module Main where

import Control.Monad
import HUnit

import ArithTests
import qualified FullSimpleTests as F
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

-- most of the tests are in into FullSimpleTests

fullsubEvalTests 
    = []

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) F.parseTests
                        , map (makeEvalTest  parseAndEval)    F.evalTests
                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests
                        , map (makeEvalTest  parseAndEval)    fullsubEvalTests
--todo                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
