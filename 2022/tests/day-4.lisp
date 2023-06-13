(in-package :aoc-2022/tests)

(def-suite day-4
  :description "Tests for Day 4 of AoC 2022."
  :in all-tests)

(in-suite day-4)

(test solution-1
      "Check the solution to Part 1."
      (is (equal 2 (day-4:solution-1 (day-4:test-path)))))

(test solution-2
      "Check the solution to Part 2."
      (is (equal 4 (day-4:solution-2 (day-4:test-path)))))

(test input-should-work
      "Parse input correctly."
      (is (equal '(((2 4) (6 8))
                   ((2 3) (4 5))
                   ((5 7) (7 9))
                   ((2 8) (3 7))
                   ((6 6) (4 6))
                   ((2 6) (4 8)))
                 (day-4:input (day-4:test-path)))))

(test range
      "Range construction"
      (is (equal '(1 2 3) (day-4:range '(1 3))))
      (is (equal '(1) (day-4:range '(1 1)))))

(test overlap-entirely-p
      "Check for complete overlap between two ranges"
      (is-true (day-4:overlap-entirely-p '((2 4) (3 4))))
      (is-true (day-4:overlap-entirely-p '((3 3) (3 4))))
      (is-true (day-4:overlap-entirely-p '((4 8) (5 7))))
      (is-false (day-4:overlap-entirely-p '((2 4) (4 6)))))

(test overlap-any-p
      "Check for any overlap between two ranges"
      (is-true (day-4:overlap-any-p '((2 8) (3 7))))
      (is-true (day-4:overlap-any-p '((5 7) (7 9))))
      (is-false (day-4:overlap-any-p '((2 4) (6 8))))
      (is-false (day-4:overlap-any-p '((6 7) (1 4)))))
