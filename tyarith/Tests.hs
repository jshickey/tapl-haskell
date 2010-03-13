module Main where

import Control.Monad
import Test.HUnit
import TaplError
import Typing
import TestUtils
import ArithTests

tyarithTypeErrorTests
    = [("predMismatch", show predMismatch, "if 0 then true else false;")
-- It's not possible to get a cond-alt type mismatch using the arith 
-- evaluator, because it will either evaluate to one of them, or else
-- fail earlier with a predMismatch
--      ,("condAltMismatch", show condAltMismatch, "if true then 0 else false;")
      ,("expectedNat 1", show expectedNat, "succ true;")
      ,("expectedNat 2", show expectedNat, "pred false;")
      ,("expectedNat 3", show expectedNat, "iszero false;")
      ]

getAllTests = do testDotFTest <- getTestDotFTest parseAndEval
                 return $ TestList $ concat [map (makeEvalTest parseAndEval) tyarithEvalTests,
                                             map (makeEvalTest parseAndEval) tyarithTypeErrorTests,
                                             [testDotFTest]
                                            ]

main :: IO ()
main = do allTests <- getAllTests
          runTests allTests


