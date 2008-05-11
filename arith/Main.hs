module Main where

import Util
import Evaluator
import TaplError

main :: IO ()
main = do contents <- getContentsFromCmdLine
          putStr $ runThrows $ parseAndEval contents
          return () 