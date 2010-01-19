module GenTests (genTests) where

import Control.Monad.Error

import Util
import TaplError
import qualified Config as C
        
genTests :: C.Config -> C.IOThrowsError ()
genTests config
    = lift $ C.writeToFile "Tests.hs"
      (base ++
       (if use_subtypes then subtype_imports else "") ++
       (if use_isorec then isorec_imports else "") ++
       (if use_equirec then equirec_imports else "") ++
       begin_tests ++
       simple_tests ++
       (if use_subtypes then subtype_tests else "") ++
       (if use_isorec then isorec_tests else "") ++
       (if use_equirec then equirec_tests else "") ++
       end_tests)
    where use_subtypes = C.hasOption config "subtypes"
          use_isorec   = C.hasType config "isorec"
          use_equirec  = C.hasType config "equirec"

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
isorec_imports  = "import qualified IsorecTests  as IT\n"
equirec_imports = "import qualified EquirecTests as ET\n"

begin_tests = "\n\
\getAllTests = do testDotFTest <- getTestDotFTestWithPath parseAndEval \"..\"\n\
\                 return $ TestList $ concat\n"
                                               
simple_tests  = "                        [ map (makeParseTest parseFullSimple) F.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)    F.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)    tyarithEvalTests\n"

subtype_tests = "                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalTests\n\
\                        , map (makeEvalTest  parseAndEval)   ST.fullsubEvalErrorTests\n"

isorec_tests  = "                        , map (makeParseTest parseFullSimple) IT.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)   IT.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)   IT.evalErrorTests\n"

equirec_tests = "                        , map (makeParseTest parseFullSimple) ET.parseTests\n\
\                        , map (makeEvalTest  parseAndEval)   ET.evalTests\n\
\                        , map (makeEvalTest  parseAndEval)   ET.evalErrorTests\n"
                                            
end_tests = "                        , [testDotFTest]\n\
\                        ]\n\
\                         \n\
\\n\
\main :: IO ()\n\
\main = getAllTests >>= runTests\n"