;;;; Day 4.

(defpackage :day-4
  (:use :cl)
  (:export :test-path
           :data-path
           :solution-1
           :solution-2
           :range
           :overlap-entirely-p
           :overlap-any-p
           :input))

(in-package :day-4)

(defun solution-1 (&optional (path (data-path)))
  "Solution to Part 1 of AoC 2022, Day 4."
  (length (remove nil (mapcar #'overlap-entirely-p (input path)))))

(defun solution-2 (&optional (path (data-path)))
  "Solution to Part 2 of AoC 2022, Day 4."
  (length (remove nil (mapcar #'overlap-any-p (input path)))))

(defun test-path ()
  "Filepath to the smaller test file for Day 4."
  "tests/data/day-4.txt")

(defun data-path ()
  "Filepath to the full input file for Day 4."
  "data/day-4.txt")

(defun input (&optional (path (data-path)))
  "Reads the Day 4 information. Returns a list of lists,
  where each list contains a two-element pair of integers
  denoting the start of the range and the end of the range.
  Eg. (((2 4) (3 4)) ((7 8) (1 3)))"
  (utils:read-file
    path
    #'(lambda (line)
        (mapcar #'(lambda (range)
                    (mapcar #'parse-integer
                            (uiop:split-string range :separator "-")))
                (uiop:split-string line :separator ",")))))

(defun overlap-entirely-p (range-pair)
  "For two pairs to overlap completely:
    1. range-1-start >= range-2-start AND range-1-end <= range-2-end
    2. range-2-start >= range-1-start AND range-2-end <= range-1-end"
  (let ((range-1-start (first (first range-pair)))
        (range-1-end (second (first range-pair)))
        (range-2-start (first (second range-pair)))
        (range-2-end (second (second range-pair))))
    (or (and (>= range-1-start range-2-start)
             (<= range-1-end range-2-end))
        (and (>= range-2-start range-1-start)
             (<= range-2-end range-1-end)))))

(defun range (pair)
  "Creates a range of numbers in a sequence from
  a pair of numbers (the range start and end).
  Input: (1 3)
  Output: (1 2 3)"
  (let* ((start (first pair))
         (end (second pair))
         (length (+ 1 (- end start))))
    (alexandria:iota length :start start)))

(defun overlap-any-p (range-pair)
  "Checks if there is any overlap in two ranges by getting the
  sequence of numbers in each range and checking the intersection."
  (let ((range-1 (range (first range-pair)))
        (range-2 (range (second range-pair))))
    (not (null (intersection range-1 range-2)))))
