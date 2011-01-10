
CC ?= gcc
LEG ?= leg
CFLAGS = -std=c99 -fPIC -I ../c_src -I deps/cspec/src -Wall
LIB = deps/cspec/build/cspec.o
BINDIR = bin
SPECS = $(shell find spec -name so_*.spec)
TESTS = $(patsubst %.spec,%.c,$(SPECS)) 
ALL_SPECS = spec/all_specs.c
TESTBIN = bin/all_specs

.SUFFIXES: .spec .c

all: compile

info:
	$(info specs=$(SPECS))

clean:
	rm -drf bin
	rm -f spec/*.c

compile: deps/cspec
	@(gcc $(CFLAGS) -o ../priv/test/bin/test_harness c_src/test_harness.c)

test: $(TESTBIN)
	@(./$(TESTBIN))

$(TESTBIN): clean $(BINDIR)
	@(cat spec/*.spec | deps/cspec/bin/cspec > $(ALL_SPECS))
	@($(CC) $(LIB) $(ALL_SPECS) $(CFLAGS) -o $(TESTBIN))

$(BINDIR):
	mkdir -p $@

#no longer in use!
.spec.c:  
	deps/cspec/bin/cspec < $*.spec > $@

deps/cspec:
	@(../rebar get-deps || echo 'ignoring non-erlang dependency version conflict...')
	make -C deps/cspec

.PHONY: deps
