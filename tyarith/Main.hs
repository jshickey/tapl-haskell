module Main where

import Util
import Typing
import TaplError

main :: IO ()
main = do contents <- getContentsFromCmdLine
          putStr $ runThrows $ parseAndEval contents
          return () 