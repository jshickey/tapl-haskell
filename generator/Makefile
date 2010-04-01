FILES = ../common/Util.hs ../common/TestUtils.hs ../common/TaplError.hs Config.hs ConfigParser.hs GenEvaluator.hs GenMain.hs GenParser.hs GenPrinting.hs GenSyntax.hs GenTests.hs GenTyping.hs GenMakefile.hs GenReadme.hs Tests.hs

INCLUDE = -i../common

f: $(FILES) ./Main.hs
	ghc $(INCLUDE) -fglasgow-exts --make ./Main.hs -o generate

test-runner: $(FILES) Tests.hs
	ghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner

test: test-runner
	./test-runner

clean:
	-rm *.hi *.o generate test-runner
