
CC ?= gcc
LEG ?= leg
CFLAGS = -std=c99 -fPIC -I ../c_src -I deps/cspec/src -Wall
LIB = deps/cspec/build/cspec.o
BINDIR = bin
SPEC_BIN = bin/spec
SPECS = $(shell find spec -name *.spec)
TESTS = $(patsubst %.spec,%.c,$(SPECS)) 
TESTBIN = bin/runtests

.SUFFIXES: .spec .c

all: compile

info:
	$(info specs=$(SPECS))

clean:
	rm -drf bin
	rm -f spec/*.c

compile: deps/cspec
	gcc $(CFLAGS) -o ../priv/test/bin/test_harness c_src/test_harness.c

test: $(TESTBIN)
	./$(TESTBIN)

$(TESTBIN): $(BINDIR) $(TESTS)
	$(CC) $(LIB) $(TESTS) $(CFLAGS) -o $(TESTBIN)

$(BINDIR):
	mkdir -p $@

.spec.c:
	deps/cspec/bin/cspec < $*.spec > $@

deps/cspec:
	../rebar get-deps || echo 'ignoring non-erlang dependency version conflict...'
	make -C deps/cspec

.PHONY: deps
