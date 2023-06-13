(defsystem "aoc-2022"
  :version "0.1.0"
  :author "Jan van Belmont"
  :depends-on ("alexandria" "cl-utilities")
  :components ((:module "src"
                :components ((:file "utils")
                             (:file "day-1")
                             (:file "day-2")
                             (:file "day-3")
                             (:file "day-4")
                             (:file "solutions"))))
  :description "AoC 2022 solutions, written in Common Lisp."
  :in-order-to ((test-op (test-op "aoc-2022/tests"))))

(defsystem "aoc-2022/tests"
  :version "0.1.0"
  :author "Jan van Belmont"
  :depends-on ("aoc-2022" "fiveam")
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "main")
                             (:file "utils")
                             (:file "day-1")
                             (:file "day-2")
                             (:file "day-3")
                             (:file "day-4"))))
  :description "Test system for AoC 2022 solutions."
  :perform (test-op (op c)
                    (symbol-call :fiveam
                                 '#:run!
                                 (find-symbol* '#:all-tests '#:aoc-2022/tests))))
