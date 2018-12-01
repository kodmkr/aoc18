(in-package :day01)

(defun read-input (path-str)
  (with-open-file (s path-str)
    (loop for line = (read-line s nil)
       while line
       collect (parse-integer line))))

(defun sol1 ()
  (let ((inp (read-input "./inputs/day01")))
    (reduce #'+ inp :initial-value 0)))

(defun doubly-occurring-freq (int-list)
  (let ((seen (make-hash-table))
        (freq 0)
        (cyc (setf (cdr (last int-list)) int-list)))
    (setf (gethash freq seen) 1)
    (loop named search for i in cyc do
         (incf freq i)
         (multiple-value-bind (num-seen already-seen?)
             (gethash freq seen)
           (declare (ignore num-seen))
           (if (not already-seen?)
               (setf (gethash freq seen) 1)
               ;; if we're here, it means that we've
               ;; already seen the freq once
               (return-from search freq))))))

(defun sol2 ()
  (-<> (read-input "./inputs/day01")
       (doubly-occurring-freq <>)))
         
