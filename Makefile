all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

ct:
	./rebar ct
