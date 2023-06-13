(in-package :aoc-2022/tests)

(def-suite day-3
  :description "Tests for Day 3 of AoC 2022."
  :in all-tests)

(in-suite day-3)

(test split-position-test
      "Test case for split-position"
      (is (equal 2 (day-3:split-position "abcd"))))

(test item-in-both-test
      "Test case for item-in-both"
      (is (equal () (day-3:item-in-both "abcdef")))
      (is (equal #\Z (day-3:item-in-both "aZbFRZ"))))

(test priority
      "Returns correct priority for character"
      (is (equal 1 (day-3:priority #\a)))
      (is (equal 26 (day-3:priority #\z)))
      (is (equal 27 (day-3:priority #\A)))
      (is (equal 52 (day-3:priority #\Z))))

(test filter-to-items
      "Filters all lines and finds items in both."
      (is (equal '(#\p #\L #\P #\v #\t #\s)
                 (day-3:filter-to-items (day-3:test-path)))))

(test solution-1
      "Returns correct solution for Part 1."
      (is (equal 157 (day-3:solution-1 (day-3:test-path)))))

(test solution-2
      "Returns correct solution for Part 2."
      (is (equal 70 (day-3:solution-2 (day-3:test-path)))))
