(in-package :aoc-2022/tests)

(def-suite day-2
  :description "Tests for Day 2 of AoC 2022."
  :in all-tests)

(in-suite day-2)

(test day-2-test
      "Test cases for day-2 functions."
      (is (equal 6 (gethash :win (day-2:score-card))))
      (is (equal 0 (gethash :lose (day-2:score-card))))
      (is (equal 3 (gethash :draw (day-2:score-card))))
      (is (equal :rock (gethash "A" (day-2:opponent-key))))
      (is (equal :paper (gethash "B" (day-2:opponent-key))))
      (is (equal :scissors (gethash "C" (day-2:opponent-key))))
      (is (equal :rock (gethash "X" (day-2:player-key))))
      (is (equal :paper (gethash "Y" (day-2:player-key))))
      (is (equal :scissors (gethash "Z" (day-2:player-key))))
      (is (equal :draw (gethash (list :rock :rock) (day-2:results-table))))
      (is (equal :lose (gethash (list :scissors :paper) (day-2:results-table))))
      (is (equal 8 (day-2:score (list "A" "Y"))))
      (is (equal '(("A" "Y") ("B" "X") ("C" "Z"))
                 (day-2:parse-lines (day-2:test-path))))
      (is (equal '(8 1 6)
                 (mapcar #'day-2:score (day-2:parse-lines (day-2:test-path)))))
      (is (equal 15 (day-2:match-score (day-2:test-path))))
      (is (equal 15 (day-2:solution-1 (day-2:test-path))))
      (is (equal 4 (day-2:prediction-score (list "A" "Y"))))
      (is (equal 1 (day-2:prediction-score (list "B" "X")))))
