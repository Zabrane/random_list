REBAR=rebar3

.PHONY: all clean test

all:
	$(REBAR) compile

edoc:
	$(REBAR) doc

clean:
	$(REBAR) clean

build_plt:
	$(REBAR) build-plt

dialyzer:
	$(REBAR) dialyze

.PHONY: run
run:
	rm -rf ./log/*
	$(REBAR) shell

.PHONY: test
test:
	rm -rf test/ct_logs
	$(REBAR) eunit 
	$(REBAR) ct


