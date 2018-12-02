(in-package :day02)

(defun read-input (path-str)
  (with-open-file (s path-str)
    (loop for line = (read-line s nil) while line
       collect line)))

;; didn't find any run-length encoding or group-by in Alexandria or
;; Serapeum, so here goes...
(defun rle (str)
  "Calculates the run-length encoding of STR.
NOTE: the string STR *must not* be empty."
  (let ((len (length str))
        (res nil))
    (loop
       for i = 0 then j
       for j = (position-if (lambda (c) (char/= c (char str i))) str :start i)
       while j
       do
         (push (cons (char str i) (- j i)) res)
       finally
         (push (cons (char str i) (- len i)) res))
    (nreverse res)))

(defun enc-all (strs)
  (let ((sorted-strs
         (loop for s in strs collect (sort s #'char<))))
    (loop for s in sorted-strs collect (sort (rle s) #'> :key (lambda (pair) (cdr pair))))))

(defun count-stuff (rles)
  (let ((twos 0)
        (threes 0))
    (loop for rle in  rles do
         (cond ((and (some (lambda (p) (= (cdr p) 2)) rle)
                     (some (lambda (p) (= (cdr p) 3)) rle))
                (incf threes)
                (incf twos))
               ((some (lambda (p) (= (cdr p) 3)) rle)
                (incf threes))
               ((some (lambda (p) (= (cdr p) 2)) rle)
                (incf twos))))
    (values threes twos)))

(defun sol1 ()
  (let* ((inp (read-input "./inputs/day02"))
         (encs (enc-all inp)))
    (multiple-value-bind (threes twos)
        (count-stuff encs)
      (* threes twos))))

(defun str-diff (str1 str2)
  (loop
     for c1 across str1
     for c2 across str2
     if (char/= c1 c2)
     collect c1))

(defun diff-by (str1 str2 &optional (delta 1))
  (let ((diff-char (str-diff str1 str2)))
    (and (= (length diff-char) delta)
         (values t (car diff-char)))))

;; Brute force solution; I guess I'm lucky that this works.
;; It stops at the first pair where the "difference" is one.
;; Will try other approaches later...
(defun sol2 ()
  (let ((strs (read-input "./inputs/day02")))
    (loop named outer for s in strs do
         (loop for u in strs do
              (multiple-value-bind (diff-by-one chr)
                  (diff-by s u)
                (when diff-by-one
                  (return-from outer (remove chr s))))))))

