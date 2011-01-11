
CC ?= gcc
LEG ?= leg
DARWIN = $(shell uname | awk '/Darwin/ { print "-D_DARWIN" }')
CFLAGS = -std=c99 -fPIC -I ../c_src -I deps/cspec/src -Werror
LIB = deps/cspec/build/cspec.o
BINDIR = bin
SPECS = $(shell find spec -name so_*.spec)
TESTS = $(patsubst %.spec,%.c,$(SPECS)) 
ALL_SPECS = spec/all_specs.c
TESTBIN = bin/all_specs
TESTLIB = bin/lib_stub.so

.SUFFIXES: .spec .c

all: compile

info:
	$(info specs=$(SPECS))

clean:
	rm -drf bin
	rm -f spec/*.c

compile: deps/cspec
	@(gcc $(CFLAGS) -o ../priv/test/bin/test_harness c_src/test_harness.c)

# TODO: make this work on windows.....
test: $(TESTBIN)
	@(env LD_LIBRARY_PATH=../priv/test/bin:$$LD_LIBRARY_PATH ./$(TESTBIN))

$(TESTLIB):
	$(CC) -fPIC -c c_src/lib_stub.c -o bin/lib_stub.o
	$(CC) -shared -o $@ bin/lib_stub.o

$(TESTBIN): clean $(BINDIR) $(TESTLIB)
	@(cat spec/*.spec | deps/cspec/bin/cspec > $(ALL_SPECS))
	@($(CC) $(LIB) $(ALL_SPECS) $(CFLAGS) $(DRV_CFLAGS) $(DARWIN) -o $(TESTBIN))

$(BINDIR):
	mkdir -p $@

#no longer in use!
.spec.c:  
	deps/cspec/bin/cspec < $*.spec > $@

deps/cspec:
	@(../rebar get-deps || echo 'ignoring non-erlang dependency version conflict...')
	make -C deps/cspec

.PHONY: deps
