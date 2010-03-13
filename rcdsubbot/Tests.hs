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

parseTests = [("whitespace1", TmRecord [], "  \t{};"),
              ("whitespace2", TmRecord [], "{};   "),
              ("whitespace2", TmRecord [], "{ } ; "),
              ("comments1", TmRecord [], "/* comment */{};"),
              ("comments2", TmRecord [], "/* comment */ {/***** foo */  }; "),
              ("comments3", TmRecord [], "/* comment ******/{};"),
              ("empty record", TmRecord [], "{};"),
              ("abs + app", TmAbs "x" TyTop (TmApp (TmVar 0 1) (TmVar 0 1)), 
                              "lambda x:Top. x x;"),
              ("app associativity", 
               TmAbs "x" TyBot (TmApp (TmApp (TmVar 0 1) (TmVar 0 1))
                                (TmVar 0 1)),
               "lambda x:Bot. x x x;")]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat
                        [ map (makeParseTest parseRcdsub)  parseTests
                        , map (makeEvalTest  parseAndEval) ST.rcdsubEvalErrorTests
                        , map (makeEvalTest  parseAndEval) ST.rcdsubEvalTests
                        , [testDotFTest]
                        ]
                         
main :: IO ()
main = getAllTests >>= runTests
