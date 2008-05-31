#   make         to rebuild the executable file f
#   make test    to rebuild the executable and run the unit tests
#   make sample  to rebuild the executable and run it on input file test.f
#                (this is "make test" in the ocaml implementation)
#   make clean   to remove all intermediate and temporary files

FILES =  Typing.hs ../fullsimple/nontyping/Syntax.hs ../fullsimple/nontyping/Printing.hs ../fullsimple/nontyping/Parser.hs ../fullsimple/nontyping/Evaluator.hs ../common/TaplError.hs ../common/Util.hs ../common/TestUtils.hs ../common/ArithTests.hs ../common/FullSimpleTests.hs ../common/SimpleContext.hs ../common/SubtypeTests.hs

INCLUDE = -i../common -i../common/HUnit-1.0 -i../fullsimple/nontyping

f: $(FILES) ../fullsimple/nontyping/Main.hs
	ghc $(INCLUDE) -fglasgow-exts --make ../fullsimple/nontyping/Main.hs -o f

test-runner: $(FILES) Tests.hs
	ghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner

test: test-runner
	./test-runner

sample: f
	./f test.f

clean:
	-rm *.hi *.o f test-runner
