module GenMakefile (genMakefile) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genMakefile :: Config -> IOThrowsError ()
genMakefile config = lift $ writeToFile "Makefile" file

file = "#   make         to rebuild the executable file f\n\
\#   make test    to rebuild the executable and run the unit tests\n\
\#   make sample  to rebuild the executable and run it on input file test.f\n\
\#                (this is \"make test\" in the ocaml implementation)\n\
\#   make clean   to remove all intermediate and temporary files\n\
\\n\
\FILES =  Typing.hs Syntax.hs Printing.hs Parser.hs Evaluator.hs ../../common/TaplError.hs ../../common/Util.hs ../../common/TestUtils.hs ../../common/ArithTests.hs ../../common/FullSimpleTests.hs ../../common/SimpleContext.hs\n\
\\n\
\INCLUDE = -i../../common -i../../common/HUnit-1.0 -i./nontyping\n\
\\n\
\f: $(FILES) Main.hs\n\
\\tghc $(INCLUDE) -fglasgow-exts --make Main.hs -o f\n\
\\n\
\test-runner: $(FILES) Tests.hs\n\
\\tghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner\n\
\\n\
\test: test-runner\n\
\\t./test-runner\n\
\\n\
\sample: f\n\
\\t./f test.f\n\
\\n\
\clean:\n\
\\t-rm *.hi *.o f test-runner \n\
\\n"
    