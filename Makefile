.PHONY: all compile clean types typecheck top

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

top:
	@erl -sname etop -hidden -setcookie img_merge -run etop \
		-node image_merge@Neptune -tracing off -output text
