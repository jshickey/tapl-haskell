module Main where

import Control.Monad
import HUnit

import ArithTests
import qualified FullSimpleTests as F
import qualified SubtypeTests as ST
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) F.parseTests
                        , map (makeEvalTest  parseAndEval)    F.evalTests
                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests
                        , map (makeEvalTest  parseAndEval)    ST.fullsubEvalTests
                        , map (makeEvalTest  parseAndEval)    ST.fullsubEvalErrorTests
                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
