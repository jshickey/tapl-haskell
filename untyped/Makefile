#   make gen     to recreate the generated files (all others depend on this)
#
#   make f       to rebuild the executable file f
#   make test    to rebuild the executable and run the unit tests
#   make sample  to rebuild the executable and run it on input file test.f
#                (this is "make test" in the ocaml implementation)
#   make clean   to remove the entire generated directory

NAME = untyped

GEN_FILE = $(NAME).gen
GEN_DIR = ../gen/$(NAME)

MAKE_IN_GEN = make --directory=$(GEN_DIR)

f: gen
	$(MAKE_IN_GEN) f

test-runner: gen
	$(MAKE_IN_GEN) test-runner

test: gen
	$(MAKE_IN_GEN) test

sample: gen f
	$(GEN_DIR)/f test.f

gen: force
	make --directory=../generator && ../generator/generate $(GEN_FILE)

force:
	true

clean:
	-rm $(GEN_DIR)/*.*; rm $(GEN_DIR)/*; rmdir $(GEN_DIR)
