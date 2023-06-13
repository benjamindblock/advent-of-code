;;;; Find the item type that appears in both
;;;; compartments of each rucksack.
;;;; What is the sum of the priorities of those item types?

(defpackage :day-3
  (:use :cl)
  (:export :split-position
           :solution-1
           :solution-2
           :test-path
           :data-path
           :item-in-both
           :filter-to-items
           :priority))

(in-package :day-3)

(defun test-path ()
  "Filepath to the smaller test file for Day 3."
  "tests/data/day-3.txt")

(defun data-path ()
  "Filepath to the full input file for Day 3."
  "data/day-3.txt")

(defun solution-1 (&optional (path (data-path)))
  (reduce #'+ (mapcar #'priority (filter-to-items path))))

(defun solution-2 (&optional (path (data-path)))
  (let* ((char-lists
           (utils:read-file path
                            #'(lambda (line)
                                (utils:string-to-chars line))))
         (groups-of-3 (utils:in-groups-of-3 char-lists))
         (badges (alexandria:flatten (mapcar #'utils:multi-intersect groups-of-3)))
         (ranks (mapcar #'priority badges)))
    (reduce #'+ ranks)))

(defun filter-to-items (path)
  (remove nil (utils:read-file path
                               #'(lambda (line)
                                   (item-in-both line)))))

(defun split-position (str)
  "Position to split the string at to get two halves."
  (/ (length str) 2))

(defun item-in-both (str)
  "Finds the item that ended up in both rucksack compartments."
  (let ((split-at (split-position str)))
    (first
      (intersection
        (utils:string-to-chars (subseq str 0 split-at))
        (utils:string-to-chars (subseq str split-at))))))

(defun priority (character)
  "Gets priority of a given character by using CL char-codes with
  a custom offset to match the AoC rules: a-z is 1-26, A-Z is 27-52."
  (let ((code (char-int character)))
    (if (<= code 90)
        (- code 38)
        (- code 96))))
