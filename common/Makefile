# make test        Tests the common code in this directory
# make all-test    Runs "make test" in all directories of this project
# make all-make    Runs "make" in all directories of this project
# make all-clean   Runs "make clean" in all directories of this project

FILES = Util.hs TaplError.hs

INCLUDE = -iHUnit-1.0

test-runner: $(FILES) Tests.hs
	ghc $(INCLUDE) --make Tests.hs -o test-runner

test: test-runner
	./test-runner

all-test: run-make-all
	./run-make-all --command=test --dir=../

all-make: run-make-all
	./run-make-all --command=make --dir=../

all-clean: run-make-all
	./run-make-all --command=clean --dir=../

run-make-all: RunMakeAll.hs
	ghc --make RunMakeAll.hs -o run-make-all

clean:
	-rm HUnit-1.0/*.hi HUnit-1.0/*.o *.hi *.o test-runner run-make-all
