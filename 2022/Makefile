PWD := $$(pwd)

test:
	rlwrap sbcl --non-interactive \
		--eval "(asdf:load-asd \"$(PWD)/aoc-2022.asd\")" \
		--eval '(ql:quickload "aoc-2022/tests")' \
		--eval '(asdf:test-system "aoc-2022")'

solve:
	rlwrap sbcl --non-interactive \
		--eval "(asdf:load-asd \"$(PWD)/aoc-2022.asd\")" \
		--eval '(ql:quickload "aoc-2022")' \
		--eval '(solutions:solutions)'
