compile:
	./rebar compile

run:
	erl -pa ebin

all: compile run
