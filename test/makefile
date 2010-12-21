
test-automake:
	cd test && ${ERL} -noshell -s automake start create src -s erlang halt -pz ${AUTOMAKE_DIR}
	cd test && ${ERL} -noshell -s automake -s erlang halt -pz ${AUTOMAKE_DIR}

erl-test-compile:
	${ERL} -noshell -s make all -s erlang halt -pa ./priv/bin -pz ./ebin -pz ./test/ebin -pz ${EUNIT_DIR} -I ./include

erl-test: erl-compile
	${ERL} -s test_suite test -pa ./priv/bin -pz ./ebin -pz ./test/ebin -pz ${EUNIT_DIR} -I ./include
