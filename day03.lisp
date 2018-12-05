(in-package :day03)

(defparameter *fabric* (make-hash-table :test #'equal))
(defparameter *side-length* 1000)

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

(defun print-fabric ()
  (loop
     for i from 0 to *side-length* do
       (loop
          for j from 0 to *side-length* do
            (multiple-value-bind (v p?)
                (gethash (cons i j) *fabric*)
              (format t "~3a " (if p? v "."))))
       (terpri)))

(defun clear-fabric ()
  (clrhash *fabric*))

(defun fabric-at (x y)
  (gethash (cons x y) *fabric*))

(defun (setf fabric-at) (v x y)
  (setf (gethash (cons x y) *fabric*) v))

(defmethod mark-fabric ((claim claim))
  (destructuring-bind (x y w h)
      (list (clm-x claim) (clm-y claim)
            (clm-w claim) (clm-h claim))
    (loop
       for i from x below (+ x w) do
         (loop
            for j from y below (+ y h) do
              (multiple-value-bind (v p?)
                  (fabric-at i j)
                (declare (ignore v))
                (if p?
                    (setf (fabric-at i j) 'x)
                    (setf (fabric-at i j) (clm-id claim))))))))

(defun mark-all-claims (claims)
  (loop for claim in claims do
       (mark-fabric claim)))

(defun count-multis (&optional (side *side-length*))
  (let ((cnt 0))
    (loop for i below side do
         (loop for j below side
            do (multiple-value-bind (v p?)
                   (fabric-at i j)
                 (when (and p? (eql v 'x))
                   (incf cnt)))))
    cnt))

(defun sol1 ()
  (let ((claims (read-input "./tests/day03"))
        (*side-length* 10))
    (clear-fabric)
    (mark-all-claims claims)
    (print-fabric)
    (count-multis)))

