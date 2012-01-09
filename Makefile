all: compile eunit

compile:
	./rebar compile

eunit:
	./rebar eunit

