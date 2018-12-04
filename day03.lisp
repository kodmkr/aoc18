(in-package :day03)

(defstruct (claim (:conc-name clm-))
  id
  ulc
  lrc)

(defun extract (line)
  (do-register-groups
      ((#'parse-integer id)
       (#'parse-integer ulc-x)
       (#'parse-integer ulc-y)
       (#'parse-integer w)
       (#'parse-integer h))
      ("^#(\\d+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)$" line)
    (return (make-claim :id id
                :ulc `(,ulc-x . ,ulc-y)
                :lrc `(,(+ ulc-x (1- w)) . ,(+ ulc-y (1- h)))))))

(defmethod area ((clm claim))
  (destructuring-bind (ulc-x . ulc-y)
      (clm-ulc clm)
    (destructuring-bind (lrc-x . lrc-y)
        (clm-lrc clm)
      (* (1+ (- lrc-x ulc-x)) (1+ (- lrc-y ulc-y))))))
  
(defmethod cap ((claim1 claim) (claim2 claim))
  (destructuring-bind ((c1-ulc-x . c1-ulc-y) (c1-lrc-x . c1-lrc-y)
                       (c2-ulc-x . c2-ulc-y) (c2-lrc-x . c2-lrc-y))
      (list (clm-ulc claim1) (clm-lrc claim1)
            (clm-ulc claim2) (clm-lrc claim2))
    (cond ((and (= c1-ulc-x c2-ulc-x) ; the claims are the same
                (= c1-ulc-y c2-ulc-y)
                (= c1-lrc-x c2-lrc-x)
                (= c1-lrc-x c2-lrc-x))
           (make-claim :id (list (clm-id claim1) (clm-id claim2))
                       :ulc (clm-ulc claim1)
                       :lrc (clm-lrc claim2)))
          ((and (< c1-ulc-x c2-ulc-x c1-lrc-x) ; claim2's ulc is inside claim1
                (< c1-ulc-y c2-ulc-y c1-lrc-y))
           (make-claim :id(list (clm-id claim1) (clm-id claim2))
                       :ulc (clm-ulc claim2)
                       :lrc (clm-lrc claim1)))
          ((and (< c2-ulc-x c1-ulc-x c2-lrc-x) ; claim1's llc is inside claim2
                (< c2-ulc-y c1-lrc-y c2-lrc-y))
           (make-claim :id(list (clm-id claim1) (clm-id claim2))
                       :ulc (cons c1-ulc-x c2-ulc-y)
                       :lrc (cons c2-lrc-x c1-lrc-y)))
          ((and (< c2-ulc-x c1-ulc-x c2-lrc-x) ; claim1's ulc is inside claim2
                (< c2-ulc-y c1-ulc-y c2-lrc-y))
           (make-claim :id(list (clm-id claim1) (clm-id claim2))
                       :ulc (clm-ulc claim1)
                       :lrc (clm-lrc claim2)))
          ((and (< c2-ulc-x c1-lrc-x c2-lrc-x) ; claim1's urc is inside claim2
                (< c2-ulc-y c1-ulc-y c2-lrc-y))
           (make-claim :id(list (clm-id claim1) (clm-id claim2))
                       :ulc (cons c2-ulc-x c1-ulc-y)
                       :lrc (cons c1-lrc-x c2-lrc-y)))
          (t nil))))

(defun read-input (path-str)
  (with-open-file (f path-str)
    (sort (loop
             for line = (read-line f nil)
             while line
             collect (extract line))
          #'> :key #'area)))

(defun do-pairwise (claims)
  (let ((tmp claims))
    (loop while (> (length tmp) 1) do
         (let* ((new (loop
                       for cdrs on tmp
                       for x in cdrs nconc
                          (loop for y in (cdr cdrs) collect (cap x y))))
                (cleaned (remove nil new)))
           (setf tmp cleaned)))
    (area (car tmp))))
         
