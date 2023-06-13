;;;; Utility functions for AoC 2022.

(defpackage :utils
  (:use :cl)
  (:export :read-file
           :string-to-chars
           :in-groups-of-2
           :in-groups-of-3
           :multi-intersect))

(in-package :utils)

(defun read-file (path &optional apply-lambda)
  "Read a file and return a list containing each line."
  (let* ((lines (uiop:split-string
                  (alexandria:read-file-into-string path)
                  :separator '(#\Newline)))
         (without-last-line (reverse (cdr (reverse lines)))))
    (if (not (null apply-lambda))
        (mapcar apply-lambda without-last-line)
        without-last-line)))

(defun in-groups-of-3 (list)
  "Input is a list. Output contains the list in groups
  of three. The last group may contain nil values at the
  end if the input list is not divisible by three."
  (loop for (val-1 val-2 val-3) on list by #'cdddr
        if (and val-1 val-2 val-3)
        collect (list val-1 val-2 val-3)))

(defun in-groups-of-2 (list)
  (loop for (val-1 val-2) on list by #'cdr
        if (not (null val-2))
          collect (list val-1 val-2)))

(defun multi-intersect (lists)
  "Input is a list of lists.
  Output is the intersection of all of those lists."
  (loop with intersected = (first lists)
        for list-pair in (in-groups-of-2 lists) do
        (setq intersected (intersection intersected (cadr list-pair)))
        finally (return (remove-duplicates intersected))))

(defun string-to-chars (str)
  "Takes a string and returns a list of its characters."
  (loop for char across str
        collect char))
