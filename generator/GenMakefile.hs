module GenMakefile (genMakefile) where

import Control.Monad.Error
import System.FilePath ((</>), splitPath, joinPath)
import System.Directory (getCurrentDirectory)
    
import Util
import TaplError
import Config
        
genMakefile :: Config -> IOThrowsError ()
genMakefile c@(CopyConfig _ files _) = create [] files
genMakefile c@(GenConfig _ _ _ _ _ _) =
    create baseGenerated $ baseCommon ++
               (if (useIsorec c) then isorecFiles else []) ++
               (if (useEquirec c) then equirecFiles else []) ++
               (if (hasSubtypes c) then subtypeFiles else [])

create :: [String] -> [String] -> IOThrowsError ()
create generated toCopy = copyFiles allToCopy >>
                          lift (writeToFile "Makefile" contents)
    where allToCopy = baseToCopy ++ toCopy
          contents = begin ++
                     "FILES = " ++
                     toList (map removePath (generated ++ toCopy)) ++
                     end
          removePath = last . splitPath
          toList = foldr1 (\x y -> x ++ " " ++ y)
          copyFiles fs = do d <- lift getCurrentDirectory
                            mapM (lift . copyToGen . (d </>)) fs

begin = "#   make         to rebuild the executable file f\n\
\#   make test    to rebuild the executable and run the unit tests\n\
\#   make sample  to rebuild the executable and run it on input file test.f\n\
\#                (this is \"make test\" in the ocaml implementation)\n\
\#   make clean   to remove all intermediate and temporary files\n\
\\n"

baseGenerated = ["Typing.hs"
                ,"Syntax.hs"
                ,"Printing.hs"
                ,"Parser.hs"
                ,"Evaluator.hs"
                ,"Main.hs"
                ,"Tests.hs"
                ]

baseToCopy = ["README"
             ,"test.f"
             ,"test.out"
             ]

baseCommon = ["../common/TaplError.hs"
             ,"../common/Util.hs"
             ,"../common/TestUtils.hs"
             ,"../common/ArithTests.hs"
             ,"../common/FullSimpleTests.hs"
             ,"../common/SimpleContext.hs"]

isorecFiles = ["../common/IsorecTests.hs"]
equirecFiles = ["../common/EquirecTests.hs"]
subtypeFiles = ["../common/SubtypeTests.hs"]

end = "\n\
\\n\
\f: $(FILES) \n\
\\tghc $(INCLUDE) -fglasgow-exts --make Main.hs -o f\n\
\\n\
\test-runner: $(FILES) \n\
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

