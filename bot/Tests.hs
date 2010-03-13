module Main where

import Control.Monad
import Test.HUnit

import ArithTests
import qualified SubtypeTests as ST
import Syntax
import TestUtils
import Evaluator
import TaplError
import Parser

parseTests = [("abs + app", TmAbs "x" TyTop (TmApp (TmVar 0 1) (TmVar 0 1)), 
                              "lambda x:Top. x x;"),
              ("app associativity", 
               TmAbs "x" TyBot (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                                (TmVar 0 1)),
               "lambda x:Bot. x x x;")]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseBot)  parseTests
                        , map (makeEvalTest  parseAndEval) ST.botEvalErrorTests
                        , map (makeEvalTest  parseAndEval) ST.botEvalTests
                        , [testDotFTest]
                        ]
                         
main :: IO ()
main = getAllTests >>= runTests
