module GenMain (genMain) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genMain :: Config -> IOThrowsError ()
genMain config = lift $ writeToFile "Main.hs" file

file = "{- fullsimple implemenation.  \n\
\\n\
\ Example Usage: \"./f -I../ test.f\" will try to evaluate the terms in\n\
\ the file \"test.f\", with \"../\" added to the search path.\n\
\ -}\n\
\module Main where\n\
\    \n\
\import Control.Monad\n\
\import Util\n\
\import Evaluator\n\
\import TaplError\n\
\\n\
\parseEvalAndPrint :: String -> IO ()\n\
\parseEvalAndPrint = putStrLn . runThrows . parseAndEval\n\
\\n\
\main :: IO ()\n\
\main = getContentsFromCmdLine >>= parseEvalAndPrint\n"                      