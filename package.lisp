;;;; package.lisp

(defpackage #:aoc18
  (:use #:cl))

(defpackage :day01
  (:use :cl :arrows))

(defpackage :day02
  (:use :cl :arrows))

(defpackage :day03
  (:use :cl :arrows)
  (:import-from :cl-ppcre :split :scan-to-strings :register-groups-bind))
