module Main where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.HUnit
import Control.Monad.Error
import Util
import TaplError
import TestUtils

testRunThrows1 = TestCase (assertEqual "Error trapped incorrectly"
                                       "Error: test msg"
                                       (runThrows $ throwError $ Default "test msg"))

testRunThrows2 = TestCase (assertEqual "Result not extracted correctly"
                                       "test output"
                                       (runThrows $ return "test output"))

testParseArgs1 = TestCase (assertEqual "Parsed args incorrectly"
                                       ("test.f", [""])
                                       (parseArgs ["test.f"]))

testParseArgs2 = TestCase (assertEqual "Parsed args incorrectly"
                                       ("test.f", ["bar", "foo", ""])
                                       (parseArgs ["-I", "foo", "test.f", "-I", "bar"]))

testFindFile = TestCase (do dir <- getCurrentDirectory
                            result <- findFile "Tests.hs" ["bad", dir]
                            assertEqual "Did not find file correctly"
                                        (dir </> "Tests.hs")
                                        result)

allTests = TestList [TestLabel "runThrows1" testRunThrows1,
                     TestLabel "runThrows2" testRunThrows2,
                     TestLabel "parseArgs1" testParseArgs1,
                     TestLabel "parseArgs2" testParseArgs2,
                     TestLabel "findFile" testFindFile]

main :: IO ()
main = runTests allTests