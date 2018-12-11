(in-package :day05)

(defun read-input (path-str)
  (string-trim '(#\Newline)
               (alexandria:read-file-into-string path-str)))

(defparameter +lowers+ "abcdefghijklmnopqrstuvwxyz")

(defun mk-regex ()
  (let* ((lowers +lowers+)
         (uls (map 'list (lambda (c) (format nil "~c~c" c (char-upcase c))) lowers)))
    (format nil "~{~a~^|~}" (loop for p in uls nconc (list p (reverse p))))))

(defun shrink (str)
  (loop do
       (multiple-value-bind (res matched?)
           (regex-replace-all (mk-regex) str "")
         (if matched?
             (setf str res)
             (return res)))))

(defun sol1 ()
  (-<> (read-input "./inputs/day05")
       (shrink <>)
       (length <>)))

(defun try-shrink (str)
  (loop
     for c across +lowers+ for uc = (char-upcase c)
     for tmp = (remove c str :test #'char-equal)
     minimize (length (shrink tmp)) into min
     finally (return min)))

(defun sol2 ()
  (-<> (read-input "./inputs/day05")
       (try-shrink <>)))
