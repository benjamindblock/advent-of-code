;;;; Print all solutions
(defpackage :solutions
  (:use :cl)
  (:export :solutions))

(in-package :solutions)

(defun solutions ()
  (format t "Day 1~%")
  (format t "Part 1: ~d~%" (day-1:solution-1))
  (format t "Part 2: ~d~%" (day-1:solution-2))
  (format t "~%Day 2 ~%")
  (format t "Part 1: ~d~%" (day-2:solution-1))
  (format t "Part 2: ~d~%" (day-2:solution-2))
  (format t "~%Day 3 ~%")
  (format t "Part 1: ~d~%" (day-3:solution-1))
  (format t "Part 2: ~d~%" (day-3:solution-2))
  (format t "~%Day 4 ~%")
  (format t "Part 1: ~d~%" (day-4:solution-1))
  (format t "Part 2: ~d~%" (day-4:solution-2)))
