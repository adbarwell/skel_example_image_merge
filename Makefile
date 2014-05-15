.PHONY: all compile clean

all: compile

compile: src/*.erl
	@erlc -o ebin src/*.erl

clean:
	rm ebin/*.beam

console: compile
	@exec erl -args_file ./priv/defaults.args
