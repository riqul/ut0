
REBAR = rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump
