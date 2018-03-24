;;; file utilities.lisp
;;; Utility functions for the finance package

(in-package :finance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date and time functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun date-string (date)
  "date format DD mon YYYY to DD-MM-YYYY"
  (let* ((ts (local-time:universal-to-timestamp (net.telent.date:parse-time date)))
         (tstr (local-time:format-timestring nil ts
                                             :format '(:day "-" :month "-" :year))))
    tstr))

(defparameter *month-ht-in-integer*
  (alexandria:plist-hash-table
   '("Jan" 1
     "Feb" 2
     "Mar" 3
     "Apr" 4
     "May" 5
     "Jun" 6
     "Jul" 7
     "Aug" 8
     "Sep" 9
     "Oct" 10
     "Nov" 11
     "Dec" 12)
   :test #'equalp))


(defun filename-date-range (filename)
  "accepts a filename form '00886XXX1871-2016Dec23-2017Jan23.xml'"
  (re:register-groups-bind (start-year start-month end-year end-month)
      ("\\w{12}-(\\d{4})(\\w{3})\\d{1,2}-(\\d{4})(\\w{3})\\d{1,2}" filename)
    (list :start-month start-month :start-year start-year :end-month end-month :end-year end-year)))

;;(filename-date-range "00886XXX1871-2016Dec23-2017Jan23.xml")

(defun parse-date-string (date-string date-range-plist)
  "Convert date string format 'dd mmm' to 'YYYY-MM-DD'"
  (re:register-groups-bind (day month)
      ("(\\d{1,2}) (\\w{3})" date-string)
    (destructuring-bind (&key start-month start-year end-month end-year) date-range-plist
      (declare (ignore end-month))
      (let ((day-number (parse-integer day :junk-allowed t))
            (month-number (gethash month *month-ht-in-integer*))
            (start-month-number (gethash start-month *month-ht-in-integer*)))
        (cond ((>= month-number start-month-number)
               (format nil "~a-~2,'0d-~2,'0d" start-year month-number day-number))
              ((<= month-number start-month-number)
               (format nil "~a-~2,'0d-~2,'0d" end-year month-number day-number)))))))


(defun handler-parse-timestring (s)
  (handler-case (clsql-sys:parse-timestring s :junk-allowed t)
    (sb-kernel:case-failure () nil)))


