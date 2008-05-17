module Main where

import Control.Monad
import HUnit

import FullSimple
import TestUtils
import Evaluator
import TaplError
import Parser

parseTests = [("true", TmTrue, "true;")
             ]

evalTests = [("true", "true : Bool", "true;")
            ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseFullSimple) parseTests
                        , map (makeEvalTest  parseAndEval)    evalTests
-- TODO                        , [testDotFTest]
                        ]
                         

main :: IO ()
main = getAllTests >>= runTests
