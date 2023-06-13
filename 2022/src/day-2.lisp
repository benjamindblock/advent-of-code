;;;; What would your total score be if everything goes exactly
;;;; according to your strategy guide?

(defpackage :day-2
  (:use :cl)
  (:export :score-card
           :opponent-key
           :player-key
           :results-table
           :score
           :match-score
           :prediction-score
           :solution-1
           :solution-2
           :parse-lines
           :test-path
           :data-path))

(in-package :day-2)

(defun test-path ()
  "Filepath to the smaller test file for Day 2."
  "tests/data/day-2.txt")

(defun data-path ()
  "Filepath to the full input file for Day 2."
  "data/day-2.txt")

(defun solution-1 (&optional (path (data-path)))
  (match-score path))

(defun solution-2 (&optional (path (data-path)))
  (reduce #'+ (mapcar #'prediction-score (parse-lines path))))

(defun match-score (path)
  (reduce #'+ (mapcar #'score (parse-lines path))))

(defun parse-lines (path)
  "Reads each line of input. Splits the line on whitespace, and removes null."
  (let ((lines
          (utils:read-file path
                           #'(lambda (line)
                               (uiop:split-string line :separator '(#\Space))))))
    (remove-if #'null lines)))

(defun score-card ()
  "Returns a score card"
  (let ((hash (make-hash-table)))
    (setf (gethash :win hash) 6)
    (setf (gethash :lose hash) 0)
    (setf (gethash :draw hash) 3)
    hash))

(defun shape-card ()
  "Returns a shape point card"
  (let ((hash (make-hash-table)))
    (setf (gethash :rock hash) 1)
    (setf (gethash :paper hash) 2)
    (setf (gethash :scissors hash) 3)
    hash))

(defun prediction-card ()
  "Returns a shape point card"
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "X" hash) :lose)
    (setf (gethash "Y" hash) :draw)
    (setf (gethash "Z" hash) :win)
    hash))

(defun prediction-results ()
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash (list :rock :win) hash) :paper)
    (setf (gethash (list :rock :draw) hash) :rock)
    (setf (gethash (list :rock :lose) hash) :scissors)
    (setf (gethash (list :paper :win) hash) :scissors)
    (setf (gethash (list :paper :draw) hash) :paper)
    (setf (gethash (list :paper :lose) hash) :rock)
    (setf (gethash (list :scissors :win) hash) :rock)
    (setf (gethash (list :scissors :draw) hash) :scissors)
    (setf (gethash (list :scissors :lose) hash) :paper)
    hash))

(defun opponent-key ()
  "Guide to the opponent's moves."
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "A" hash) :rock)
    (setf (gethash "B" hash) :paper)
    (setf (gethash "C" hash) :scissors)
    hash))

(defun player-key ()
  "Guide to the player's moves."
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "X" hash) :rock)
    (setf (gethash "Y" hash) :paper)
    (setf (gethash "Z" hash) :scissors)
    hash))

(defun results-table ()
  "Player result for each pair of moves. Opponent move is first."
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash (list :rock :rock) hash) :draw)
    (setf (gethash (list :rock :paper) hash) :win)
    (setf (gethash (list :rock :scissors) hash) :lose)
    (setf (gethash (list :paper :rock) hash) :lose)
    (setf (gethash (list :paper :paper) hash) :draw)
    (setf (gethash (list :paper :scissors) hash) :win)
    (setf (gethash (list :scissors :rock) hash) :win)
    (setf (gethash (list :scissors :paper) hash) :lose)
    (setf (gethash (list :scissors :scissors) hash) :draw)
    hash))

(defun score (line)
  "Gets a score for line of input, represented as: ('A' 'Y')."
  (let* ((opponent-move (gethash (car line) (opponent-key)))
         (player-move (gethash (cadr line) (player-key)))
         (shape-bonus (gethash player-move (shape-card)))
         (decision (gethash (list opponent-move player-move) (results-table))))
    (+ (gethash decision (score-card)) shape-bonus)))

(defun prediction-score (line)
  (let* ((opponent-move (gethash (car line) (opponent-key)))
         (prediction (gethash (cadr line) (prediction-card)))
         (player-move (gethash (list opponent-move prediction) (prediction-results)))
         (shape-bonus (gethash player-move (shape-card)))
         (score-value (gethash prediction (score-card))))
    (+ score-value shape-bonus)))
