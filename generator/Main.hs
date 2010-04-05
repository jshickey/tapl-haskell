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
import GenReadme

generate :: Config -> IOThrowsError ()
generate c@(CopyConfig _ _ _) = genMakefile c
generate c@(GenConfig _ _ _ _ _ _) = genEvaluator c >>
                                   genMain c >>
                                   genParser c >>
                                   genPrinting c >>
                                   genSyntax c >>
                                   genTyping c >>
                                   genTests c >> 
                                   genMakefile c >>
                                   genReadme c
    
parseAndGen :: String -> IOThrowsError ()
parseAndGen str = liftThrows (parseConfig str) >>=
                  generate >>
                  lift (putStrLn "Successfully generated files.")
    
main :: IO ()
main = do str <- getContentsFromCmdLine
          ret <- runErrorT (parseAndGen str)
          case ret of
            Left err -> error $ show err
            Right _ -> return ()
          
