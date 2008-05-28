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

-- A lot of the tests are inherited from FullSimpleTests.  The ones 
-- specifically for references are listed below.

fullErrorParseTests 
    = [ 
      ]

fullErrorEvalTests 
    = [ 
      ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullError) F.parseTests
                        , map (makeEvalTest  parseAndEval)   F.evalTests
                        , map (makeParseTest parseFullError) fullErrorParseTests
                        , map (makeEvalTest  parseAndEval)   fullErrorEvalTests
                        , map (makeEvalTest  parseAndEval)   tyarithEvalTests
-- todo                        , [testDotFTest]
                        ]
                         
main :: IO ()
main = getAllTests >>= runTests
