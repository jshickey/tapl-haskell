module Main where

import Control.Monad.Error

import Util
import TaplError
    
import ConfigParser
import Config
import GenEvaluator
import GenMain
import GenParser
import GenPrinting
import GenSyntax
import GenTyping
import GenTests
import GenMakefile
    
parseAndGen :: String -> IOThrowsError ()
parseAndGen str = do config <- liftThrows $ parseConfig str
                     genEvaluator config
                     genMain config
                     genParser config
                     genPrinting config
                     genSyntax config
                     genTyping config
                     genTests config
                     genMakefile config
                     lift $ putStrLn "Successfully generated files."
    
main :: IO ()
main = do str <- getContentsFromCmdLine
          ret <- runErrorT (parseAndGen str)
          case ret of
            Left err -> error $ show err
            Right _ -> return ()
          
