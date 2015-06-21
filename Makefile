compile:
	./rebar3 do compile

run:
	./rebar3 do shell

clean:
	./rebar3 clean

all: compile run
