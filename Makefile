
ERL ?= `which erl`
VERBOSE ?= ""
HERE := $(shell pwd)

all: info clean inttest

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

compile: precompile
	@(env ERL_LIBS=$$ERL_LIBS ERL_INCLUDES=c_src:$$ERL_INCLUDES ./rebar $$VERBOSE compile skip_deps=true)

precompile:
	@(env ERL_LIBS=$$ERL_LIBS ./rebar check-deps skip_deps=true)

clean:
	@(./rebar clean skip_deps=true)
	make -C inttest clean

distclean: clean
	./rebar delete-deps

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@(dialyzer -Wno_return -c apps/riak_core/ebin)

inttest: test
	make -C inttest test

test: compile pretest ct

ct:
	@(env ERL_LIBS=$$ERL_LIBS LD_LIBRARY_PATH=./priv:./priv/test/bin:$$LD_LIBRARY_PATH rebar $$VERBOSE ct skip_deps=true)

pretest:
	cd inttest && make -f Makefile

.PHONY: deps test inttest
