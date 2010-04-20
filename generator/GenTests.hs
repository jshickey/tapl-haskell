module GenTests (genTests) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genTests :: Config -> IOThrowsError ()
genTests c
    = lift $ writeToFile "Tests.hs"
      (base ++
       (if (hasSubtypes c) then subtype_imports else "") ++
       (if (useIsorec c) then isorec_imports else "") ++
       (if (useEquirec c) then equirec_imports else "") ++
       begin_tests ++
       simple_tests ++
       (if (hasSubtypes c) then subtype_tests else "") ++
       (if (useIsorec c) then isorec_tests else "") ++
       (if (useEquirec c) then equirec_tests else "") ++
       end_tests)

base = "module Main where\n\
\\n\
\import Control.Monad\n\
\import Test.HUnit\n\
\\n\
\import ArithTests\n\
\import qualified FullSimpleTests as F\n\
\import Syntax\n\
\import TestUtils\n\
\import Evaluator\n\
\import TaplError\n\
\import Parser\n"

subtype_imports = "import qualified SubtypeTests as ST\n"
isorec_imports  = "import qualified IsorecTests  as IT\n"
equirec_imports = "import qualified EquirecTests as ET\n"

begin_tests = "\n\
\getAllTests = do testDotFTest <- getTestDotFTest parseAndEval\n\
\                 return $ TestList $ concat\n"
                                               
simple_tests  = "                        [ map (makeParseTest parseFullSimple) F.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)    F.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests\n"

subtype_tests = "                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalTests\n\
\                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalErrorTests\n"

isorec_tests  = "                        , map (makeEvalTest  parseAndEval)   IT.evalTests\n"

equirec_tests = "                        , map (makeEvalTest  parseAndEval)   ET.evalTests\n"
                                            
end_tests = "                        , [testDotFTest]\n\
\                        ]\n\
\                         \n\
\\n\
\main :: IO ()\n\
\main = getAllTests >>= runTests\n"