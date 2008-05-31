#   make         to rebuild the executable file f
#   make test    to rebuild the executable and run the unit tests
#   make sample  to rebuild the executable and run it on input file test.f
#                (this is "make test" in the ocaml implementation)
#   make clean   to remove all intermediate and temporary files

FILES =  Typing.hs ./nontyping/Syntax.hs ./nontyping/Printing.hs ./nontyping/Parser.hs ./nontyping/Evaluator.hs ../common/TaplError.hs ../common/Util.hs ../common/TestUtils.hs ../common/ArithTests.hs ../common/FullSimpleTests.hs ../common/SimpleContext.hs

INCLUDE = -i../common -i../common/HUnit-1.0 -i./nontyping

f: $(FILES) ./nontyping/Main.hs
	ghc $(INCLUDE) -fglasgow-exts --make ./nontyping/Main.hs -o f

test-runner: $(FILES) Tests.hs
	ghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner

test: test-runner
	./test-runner

sample: f
	./f test.f

clean:
	-rm *.hi *.o f test-runner ./nontyping/*.hi ./nontyping/*.o
