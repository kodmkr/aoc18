(in-package :day04)

(defstruct date
  year
  month
  day)

(defstruct (tim (:include date))
  hour
  min)

(defstruct (event (:include tim))
  evt)

(defun extract (line)
  (let ((time-part "^\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] ")
        (event-part ".*?#?(\\d+|asleep|wakes).*"))
    (register-groups-bind ((#'parse-integer y m d hr min) event)
        ((concatenate 'string time-part event-part) line)
      (labels ((act (evt)
                 (case (char evt 0)
                   (#\a 'a)
                   (#\w 'w)
                   (t (parse-integer evt)))))
        (make-event :year y :month m :day d :hour hr :min min :evt (act event))))))

(defun read-input (path-str)
  (with-open-file (f path-str)
    (let ((uns (loop
                  for line = (read-line f nil)
                  while line
                  collect (extract line))))
      (labels ((pred (evt1 evt2)
                 (or (< (event-year evt1) (event-year evt2))
                     (and (= (event-year evt1) (event-year evt2))
                          (< (event-month evt1) (event-month evt2)))
                     (and (= (event-year evt1) (event-year evt2))
                          (= (event-month evt1) (event-month evt2))
                          (< (event-day evt1) (event-day evt2)))
                     (and (= (event-year evt1) (event-year evt2))
                          (= (event-month evt1) (event-month evt2))
                          (= (event-day evt1) (event-day evt2))
                          (< (event-hour evt1) (event-hour evt2)))
                     (and (= (event-year evt1) (event-year evt2))
                          (= (event-month evt1) (event-month evt2))
                          (= (event-day evt1) (event-day evt2))
                          (= (event-hour evt1) (event-hour evt2))
                          (< (event-min evt1) (event-min evt2))))))
        (sort uns #'pred)))))

(defun asleep-times (events)
  (let ((time-table (make-hash-table))
        (totals (make-hash-table))
        curr-id
        start)
    (loop
       for event in events
       if (integerp (event-evt event)) do
         (setf curr-id (event-evt event))
         (if (null (gethash curr-id time-table))
             (setf (gethash curr-id time-table) (list nil))
             (push nil (gethash curr-id time-table)))
         (when (null (gethash curr-id totals))
           (setf (gethash curr-id totals) 0))
       else if (eql (event-evt event) 'A) do
         (setf start (event-min event))
       else if (eql (event-evt event) 'W) do
         (push (cons start (event-min event))
               (car (gethash curr-id time-table)))
         (incf (gethash curr-id totals) (- (event-min event) start)))
    (sort (loop
                  for id being the hash-key of time-table
                  collect (list id
                                (gethash id totals)
                                (nreverse (loop
                                             for pairsl in (gethash id time-table)
                                             collect (nreverse pairsl)))))
               #'> :key #'cadr)))

(defun max-minute (times)
  (let ((time-line (make-array 60 :initial-element 0)))
    (loop for times-of-day in times do
         (loop for (start . end) in times-of-day do
              (loop for i from start to end do
                   (incf (aref time-line i)))))
    (loop
       with max = -1
       with min
       for i below 60 for val = (aref time-line i)
       if (> val max) do
         (setf max val
               min i)
       finally (return (cons min max)))))
         
(defun sol1 ()
  (let* ((inpt (read-input "./inputs/day04"))
         (asleep-times (car (asleep-times inpt)))
         (id (car asleep-times))
         (min+cnt (max-minute (caddr asleep-times))))
    (* id (car min+cnt))))

(defun sol2 ()
  (let* ((inpt (read-input "./inputs/day04"))
         (asleep-times (asleep-times inpt))
         (max-asleep (car (sort (loop for (id time lst) in asleep-times
                                   collect (list id (max-minute lst)))
                                #'> :key #'cdadr))))
    (* (car max-asleep) (caadr max-asleep))))

