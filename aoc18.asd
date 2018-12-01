;;;; aoc18.asd

(asdf:defsystem #:aoc18
  :description "Advent of Code 2018"
  :depends-on ("cl-ppcre" "arrows")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "aoc18")))
