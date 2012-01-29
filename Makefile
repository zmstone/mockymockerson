all: deps compile ct

deps:
	./rebar get-deps

compile:
	./rebar compile

ct:
	./rebar ct
