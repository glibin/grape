all:
	./rebar get-deps compile

app:
	@./rebar compile

clean:
	@./rebar clean
	@rm -f erl_crash.dump

run:
	# ulimit -n 10240
	ERL_MAX_PORTS=10240 ERL_LIBS=..:deps erl -name grape@127.0.0.1 -pa ebin -s grape_launcher

1:
	# ulimit -n 10240
	ERL_MAX_PORTS=10240 ERL_LIBS=..:deps erl -name node1@127.0.0.1 -pa ebin -s grape_example

2:
	# ulimit -n 10240
	ERL_MAX_PORTS=10240 ERL_LIBS=..:deps erl -name node2@127.0.0.1 -pa ebin -s grape_example

ct: test

test:
	mkdir -p logs
	ct_run -pa ebin -pa deps/*/ebin -logdir logs/ -dir test/

.PHONY: test
