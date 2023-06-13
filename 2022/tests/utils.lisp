(in-package :aoc-2022/tests)

(def-suite utils
           :description "Tests for utility functions"
           :in all-tests)

(in-suite utils)

(test read-file
      "Test cases for utilities"
      (is (equal '("1" "2" "" "3" "4" "" "6")
                 (utils:read-file "tests/data/day-1.txt")))
      (is (equal '(1 2 nil 3 4 nil 6)
                 (utils:read-file "tests/data/day-1.txt"
                                  #'(lambda (line)
                                      (when (not (string= "" line))
                                        (parse-integer line))))))
      (is (equal '(("2-4" "6-8")
                   ("2-3" "4-5")
                   ("5-7" "7-9")
                   ("2-8" "3-7")
                   ("6-6" "4-6")
                   ("2-6" "4-8"))
                 (utils:read-file "tests/data/day-4.txt"
                                  #'(lambda (line)
                                      (uiop:split-string line :separator ","))))))

(test in-groups-of-3
      "Returns groups of 3"
      (is (equal '((1 2 3) (4 5 6))
                 (utils:in-groups-of-3 '(1 2 3 4 5 6 7))))
      (is (equal '(((#\a #\b #\c) (#\d #\e #\f) (#\g #\h #\i)))
                 (utils:in-groups-of-3
                   (mapcar #'utils:string-to-chars
                           '("abc" "def" "ghi"))))))

(test in-groups-of-2
      "Returns groups of 2"
      (is (equal '((1 2) (2 3) (3 4))
                 (utils:in-groups-of-2 '(1 2 3 4)))))

(test multi-intersect
      "Return the intersection of multiple lists."
      (is (equal '(3)
                 (utils:multi-intersect '((1 2 3) (2 5 3) (7 8 3)))))
      (is (equal '(#\a)
                 (utils:multi-intersect '((#\a) (#\Z #\a) (#\H #\a)))))
      (is (equal '(#\r)
                 (utils:multi-intersect '((#\v #\J #\r #\w #\p #\W #\t #\w)
                                          (#\j #\q #\H #\R #\N #\q #\r #\j #\q)
                                          (#\P #\m #\m #\d #\z #\q #\P #\r))))))

(test string-to-chars-test
      "Test cases for string-to-chars."
      (is (equal '(#\a #\b #\c)
                 (utils:string-to-chars "abc")))
      (is (equal '((#\a #\b) (#\c))
                 (mapcar #'utils:string-to-chars '("ab" "c")))))
