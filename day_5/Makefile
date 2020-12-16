all: compile run

clean: _build
	rm -rf _build
	rm -f day_5

compile:
	rebar3 escriptize
	cp _build/default/bin/day_5 .

run: ./day_5
	./day_5 puzzle_input.txt
