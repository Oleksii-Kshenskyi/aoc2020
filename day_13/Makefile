INPUTS=sample.txt sample2.txt sample3.txt sample4.txt sample5.txt sample6.txt puzzle_input.txt

all: compile run

clean: _build day_13
	rm -rf _build day_13

compile:
	rebar3 escriptize
	cp _build/default/bin/day_13 .

run: ./day_13
	./day_13 $(INPUTS)