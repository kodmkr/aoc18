(in-package :day03)

(defparameter *fabric* (make-hash-table :test #'equal))

(defstruct (claim (:conc-name clm-))
  id
  x y
  w h)

(defun extract (line)
  (register-groups-bind
      ((#'parse-integer id x y w h))
      ("^#(\\d+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)$" line)
    (make-claim :id id :x x :y y :w w :h h)))

(defmethod area ((clm claim))
  (destructuring-bind (x y w h)
      `(,(clm-x clm) ,(clm-y clm) ,(clm-w clm) ,(clm-h clm))
    (declare (ignore x y))
    (* w h)))

(defun read-input (path-str)
  (with-open-file (f path-str)
    (sort (loop
             for line = (read-line f nil)
             while line
             collect (extract line))
          #'> :key #'area)))

