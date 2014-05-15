.PHONY: all compile clean types typecheck

all: compile

compile: src/*.erl
	@erlc -o ebin src/*.erl

clean:
	rm ebin/*.beam

console: compile
	@exec erl -args_file ./priv/defaults.args

types:
	@typer src/*.erl

typecheck:
	@dialyzer src/*.erl
