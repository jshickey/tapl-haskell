#   make         to rebuild the executable file f
#   make test    to rebuild the executable and run the unit tests
#   make sample  to rebuild the executable and run it on input file test.f
#                (this is "make test" in the ocaml implementation)
#   make clean   to remove all intermediate and temporary files

FILES = Syntax.hs Printing.hs Context.hs Typing.hs Parser.hs Evaluator.hs ../common/TaplError.hs ../common/Util.hs ../common/TestUtils.hs ../common/ArithTests.hs

INCLUDE = -i../common -i../common/HUnit-1.0

f: $(FILES) Main.hs
	ghc $(INCLUDE) -fglasgow-exts --make Main.hs -o f

test-runner: $(FILES) Tests.hs
	ghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner

test: test-runner
	./test-runner

sample: f
	./f test.f

clean:
	-rm *.hi *.o f test-runner
