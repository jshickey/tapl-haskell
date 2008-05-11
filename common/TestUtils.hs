{- Helper functions for creating test cases
 -}
module TestUtils where

import HUnit
import TaplError
import Control.Monad
import System.Exit
import Util

makeEvalTest parseAndEval (label, expected, input) = 
    TestLabel label
              (TestCase (assertEqual "Incorrect evaluation" 
                                     (expected ++ "\n")
                                     (runThrows (parseAndEval input))))

makeParseTest parser (label, expected, input) = 
    TestLabel label (TestCase testCase)
        where testCase = case parser input of
                           Left err -> assertFailure $ show err
                           Right [term] -> assertEqual "Incorrect parse" expected term

-- Each OCaml implementation has a test.f that we copy over into our
-- implementation, along with a test.out file that contains the output
-- from running it on the OCaml impl
getTestDotFTest parseAndEval
    = do input <- getFileContents "test.f"
         output <- getFileContents "test.out"
         return $ makeEvalTest parseAndEval 
                    ("test.f", 
                     -- drop the last newline, since makeEvalTest assumes
                     -- that the test case was created without it
                     take ((length output) - 1) output, 
                     input)

-- Runs a list of tests, and exits the program with an error code
-- if there are any failures or errors
runTests :: Test -> IO ()
runTests allTests = do counts <- runTestTT allTests
                       putStrLn $ show counts
                       if ((errors counts) + (failures counts)) > 0
                         then exitFailure
                         else return ()