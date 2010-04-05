module GenReadme (genReadme) where

import Control.Monad.Error
    
import Util
import TaplError
import Config

genReadme :: Config -> IOThrowsError ()
genReadme c = lift $ writeToFile "README" $
              (beginReadme (name c)) ++ endReadme ++
              if ((notes c) == "") then "" else ("\n\nNOTES:\n\n" ++ (notes c))

beginReadme name = "Haskell port of the " ++ name ++
                   " OCaml implementation of TAPL.\n"
                
endReadme = "\\n\
\Sample usage:\n\
\\n\
\> make test\n\
\\n\
\> make\n\
\> ./f test.f\n\
\\n\
\See the Makefile for all targets.\n"
                                     