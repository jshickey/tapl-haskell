module GenTests (genTests) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genTests :: Config -> IOThrowsError ()
genTests config = lift $ writeToFile "Tests.hs" file

file = "module Main where\n\
\\n\
\import Control.Monad\n\
\import HUnit\n\
\\n\
\import ArithTests\n\
\import qualified FullSimpleTests as F\n\
\import Syntax\n\
\import TestUtils\n\
\import Evaluator\n\
\import TaplError\n\
\import Parser\n\
\\n\
\-- most of the tests have been moved up into FullSimpleTests\n\
\\n\
\getAllTests = do testDotFTest <- getTestDotFTestWithPath parseAndEval \"..\"\n\
\                 return $ TestList $ concat\n\
\                        [ map (makeParseTest parseFullSimple) F.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)    F.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests\n\
\                        , [testDotFTest]\n\
\                        ]\n\
\                         \n\
\\n\
\main :: IO ()\n\
\main = getAllTests >>= runTests\n"                      