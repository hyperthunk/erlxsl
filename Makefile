
.PHONY: deps test

ERL ?= `which erl`
VERBOSE ?= ""
HERE := $(shell pwd)

all: info clean compile

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

precompile:
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE check-deps skip_deps=true)

compile: precompile
	@(env ERL_LIBS=$$ERL_LIBS ERL_INCLUDES=c_src:$$ERL_INCLUDES ./rebar $$VERBOSE compile skip_deps=true)

clean:
	@(./rebar clean skip_deps=true)

distclean: clean 
	./rebar delete-deps

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@(dialyzer -Wno_return -c apps/riak_core/ebin)

test: compile
	@(env ERL_LIBS=$$ERL_LIBS LD_LIBRARY_PATH=./priv:./priv/test/bin:$$LD_LIBRARY_PATH rebar $$VERBOSE ct skip_deps=true)
