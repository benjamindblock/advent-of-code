;;;; Find the Elf carrying the most Calories.
;;;; How many total Calories is that Elf carrying?

(defpackage :day-1
  (:use :cl)
  (:export :solution-1
           :solution-2
           :group-elf-calories
           :read-as-integer
           :test-path
           :data-path
           :n-max-calories))

(in-package :day-1)

(defun test-path () "tests/data/day-1.txt")

(defun data-path () "data/day-1.txt")

(defun read-as-integer (path)
  (utils:read-file path
                   #'(lambda (line)
                       (when (not (string= "" line))
                         (parse-integer line)))))

(defun solution-1 (&optional (path (data-path)))
  "Solution to Part 1. By default uses the data text file."
  (max-calories
    (group-elf-calories
      (read-as-integer path))))

(defun solution-2 (&optional (path (data-path)))
  "Solution to Part 2. By default uses the data text file."
  (let ((max-caloric-packs (n-max-calories
                             (group-elf-calories
                               (read-as-integer path))
                             3)))
    (reduce #'+ max-caloric-packs)))

(defun group-elf-calories (sequence)
  (remove-if #'null (cl-utilities:split-sequence NIL sequence)))

(defun sum-groups (grouped)
  (mapcar #'(lambda (group) (reduce '+ group)) grouped))

(defun max-calories (grouped)
  (reduce #'max (sum-groups grouped)))

(defun n-max-calories (grouped num)
  (subseq (sort (sum-groups grouped) #'>) 0 num))
