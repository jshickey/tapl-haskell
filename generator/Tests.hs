module Main where

import Control.Monad
import Test.HUnit

import TestUtils

-- TODO: add some unit tests of the generator
-- The main tests of the generator are that the generated
-- implementations pass their own tests.
getAllTests = return $ TestList []
                         
main :: IO ()
main = getAllTests >>= runTests
