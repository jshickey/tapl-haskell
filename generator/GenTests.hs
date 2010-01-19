module GenTests (genTests) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genTests :: Config -> IOThrowsError ()
genTests config
    = lift $ writeToFile "Tests.hs"
      (base ++
       (if (subtypes (options config)) then subtype_imports else "") ++
       begin_tests ++
       simple_tests ++
       (if (subtypes (options config)) then subtype_tests else "") ++
       end_tests)

base = "module Main where\n\
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
\import Parser\n"

subtype_imports = "import qualified SubtypeTests as ST\n"
                 
begin_tests = "\n\
\getAllTests = do testDotFTest <- getTestDotFTestWithPath parseAndEval \"..\"\n\
\                 return $ TestList $ concat\n"
                                               
simple_tests = "                        [ map (makeParseTest parseFullSimple) F.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)    F.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests\n"

subtype_tests = "                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalTests\n\
\                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalErrorTests\n"
                                            
end_tests = "                        , [testDotFTest]\n\
\                        ]\n\
\                         \n\
\\n\
\main :: IO ()\n\
\main = getAllTests >>= runTests\n"