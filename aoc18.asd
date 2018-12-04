;;;; aoc18.asd

(asdf:defsystem #:aoc18
  :description "Advent of Code 2018"
  :depends-on ("cl-ppcre" "arrows" "alexandria" "serapeum")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "aoc18")))
