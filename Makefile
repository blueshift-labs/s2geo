REBAR=rebar3

nif_compile:
	@./build_deps.sh
	@make V=0 -C c_src -j 8

nif_clean:
	@make -C c_src clean

compile:
	${REBAR} compile

clean:
	${REBAR} clean

eunit:
	${REBAR} eunit

ct:
	mkdir -p log
	ct_run -suite test_s2cellid -pa ebin -pa deps/*/ebin -include include -include src -include priv -logdir log
