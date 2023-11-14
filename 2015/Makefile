.PHONY: all clean

include session.sh

DATA = day01/input.txt \
			 day02/input.txt \
			 day03/input.txt \
			 day04/input.txt \
			 day05/input.txt \
			 day06/input.txt \
			 day07/input.txt

# day01/input.txt => day/1
PARSE = $(shell echo "$@" \
	| sed 's/\/input.txt//' \
	| sed 's/./&\//3' \
	| sed 's/\/0/\//')
SET_URL = $(eval URL=$(PARSE))

# TODO: Run each solution here.
all: $(DATA)

# Downloads all the input files.
%/input.txt:
	@mkdir -p $(shell dirname "$@")
	$(SET_URL)
	@curl "https://adventofcode.com/2015/$(URL)/input" --silent \
		--cookie "$(SESSION)" -o "$@" -A "https://benjamindblock.com"

# Removes all data files.
clean:
	@rm **/input.txt

# Debug to print variables.
print-%  : ; @echo $* = $($*)
