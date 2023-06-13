(in-package :aoc-2022/tests)

(def-suite day-1
           :description "Tests for Day 1 of AoC 2022."
           :in all-tests)

(in-suite day-1)

(test day-1-test
      "Test cases for day-1 functions."
      (is (equal '((1 2) (3 4) (6))
                 (day-1:group-elf-calories
                   (day-1:read-as-integer (day-1:test-path)))))
      (is (equal '(7 6)
                 (day-1:n-max-calories
                   (day-1:group-elf-calories
                     (day-1:read-as-integer (day-1:test-path)))
                   2))))
