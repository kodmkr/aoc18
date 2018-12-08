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

(defun read-input (path-str)
  (with-open-file (f path-str)
    (loop
       for line = (read-line f nil)
       while line
       collect (extract line))))

;; for debugging purposes
(defun print-fabric (&optional (side-length *side-length*))
  (loop
     for i from 0 to side-length do
       (loop
          for j from 0 to side-length do
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
      (list (clm-x claim) (clm-y claim) (clm-w claim) (clm-h claim))
    (loop
       for i from x below (+ x w) do
         (loop
            for j from y below (+ y h) do
              (multiple-value-bind (v p?)
                  (fabric-at j i)
                (declare (ignore v))
                (setf (fabric-at j i) (if p? 'x (clm-id claim))))))))

(defun mark-all-claims (claims)
  (loop for claim in claims do
       (mark-fabric claim)))

(defun count-multis (&optional (side *side-length*))
  (let ((cnt 0))
    (loop for i below side do
         (loop for j below side
            do (multiple-value-bind (v p?)
                   (fabric-at j i)
                 (when (and p? (eql v 'x))
                   (incf cnt)))))
    cnt))

(defun sol1 ()
  (let ((claims (read-input "./inputs/day03")))
    (clear-fabric)
    (mark-all-claims claims)
    (count-multis)))

(defmethod overlap-p ((c1 claim) (c2 claim))
  (destructuring-bind (x1 y1 w1 h1 m1 n1 w2 h2)
      (list (clm-x c1) (clm-y c1) (clm-w c1) (clm-h c1)
            (clm-x c2) (clm-y c2) (clm-w c2) (clm-h c2))
    (let ((x2 (+ x1 w1))
          (y2 (+ y1 h1))
          (m2 (+ m1 w2))
          (n2 (+ n1 h2)))
      (or (and (or (and (<= x1 m1) (< m1 x2))
                   (and (< x1 m2) (<= m2 x2)))
               (or (and (<= y1 n1) (< n1 y2))
                   (and (< y1 n2) (<= n2 y2))))
          (and (< m1 x1) (< x2 m2) (< y1 n1) (< n2 y2))))))

(defun do-pairwise (claims)
  (let ((map (make-hash-table)))
    (loop for claim in claims do
         (setf (gethash (clm-id claim) map) 0))
    (loop
       for cdrs on claims
       for a in cdrs nconc
         (loop
            for b in (cdr cdrs)
            if (or (overlap-p a b) (overlap-p b a))
            do
              (setf (gethash (clm-id a) map) 1
                    (gethash (clm-id b) map) 1)))
    (loop for k being each hash-key of map
       using (hash-value val)
       if (zerop val) collect k)))

(defun sol2 ()
  (let* ((claims (read-input "./inputs/day03"))
         (singletons (do-pairwise claims)))
    (when (= 1 (length singletons))
      (car singletons))))
