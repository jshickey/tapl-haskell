module Main where
    
import Control.Monad
import Util
import Evaluator
import TaplError

parseEvalAndPrint :: String -> IO ()
parseEvalAndPrint = putStrLn . runThrows . parseAndEval

main :: IO ()
main = getContentsFromCmdLine >>= parseEvalAndPrint
       
    