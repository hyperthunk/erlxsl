# Environment
OTP_TOP=/opt/erlang
OTP_BIN=/usr/local/bin
ERL=/usr/local/bin/erl

EXTERNALS=/usr/lib
AUTOMAKE_DIR=../extern/lib/automake/ebin

all: clean build test

build: build-native erl-compile

build-native: clean
	cd c_src && make build

erl-compile:
	${ERL} -noshell -s make all -s erlang halt -pa ./priv/bin -I ./include

erl-test: erl-compile
	${ERL} -s test_suite test -pa ./priv/bin -pz ./ebin -pz ./test/ebin -I ./include

clean: clean-ebin
	cd c_src && make -f Makefile clean

clean-ebin:
	cd ebin/erlxsl && rm -rfv **/*.beam 

automake:
	${ERL} -noshell -s automake start create src -s erlang halt -pz ${AUTOMAKE_DIR}
	${ERL} -noshell -s automake -s erlang halt -pz ${AUTOMAKE_DIR}

win32-cleanup:
	sfc /cachesize=0

#-noshell -make all noexec -s erlang halt -pz ${EUNIT_DIR}/ebin -pz ${EXTERNALS}/lib
