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
   '("JAN" 1
     "FEB" 2
     "MAR" 3
     "APR" 4
     "MAY" 5
     "JUN" 6
     "JUL" 7
     "AUG" 8
     "SEP" 9
     "OCT" 10
     "NOV" 11
     "DEC" 12)
   :test #'equalp))

;;(filename-date-range "00886XXX1871-2016Dec23-2017Jan23.xml")

(defun parse-visa-date-string (date-string year-string)
  "Convert date string format 'mmm dd' to 'mmm-DD-YYYY'"
  (re:register-groups-bind (month-string day-string)
      ("(\\w{3}) (\\d{2})" date-string)
    (format nil "~a ~d ~d" month-string day-string year-string)))

(parse-visa-date-string "OCT 24" "2022")

(defun parse-bank-date-string (date-string year-string)
  "Convert date string format 'mmm dd' to 'mmm-DD-YYYY'"
  (re:register-groups-bind (day-string month-string )
      ("(\\d{1,2}) (\\w{3})" date-string)
    (format nil "~a ~2,'0d ~d" month-string (parse-number:parse-number day-string) year-string)))

(parse-bank-date-string "5 Oct" "2022")

(defun handler-parse-timestring (s)
  (handler-case (clsql-sys:parse-timestring s :junk-allowed t)
    (sb-kernel:case-failure () nil)))

(defun handler-parse-number (string)
  "Convert string to number"
  (let ((no-commas (remove #\, string )))
    (handler-case (parse-number:parse-number no-commas)
      (parse-error () nil)
      (type-error () nil))))

(defun dollars-parse (line)
  "convert a string representing a dollar amount to a numerical value"
  (let* ((dollars (re:create-scanner
		   "^(-?)\\$(\\d*,*\\d{0,3}\\.\\d{2})$")))
    (re:register-groups-bind (sign digits-only)
	(dollars line)
      (if (equal sign "-")
      	  (- (handler-parse-number digits-only))
	  (handler-parse-number digits-only)))))

(defun datep (line)
  "date predicate"
  (let ((bank-date (re:create-scanner
		    "^\\d{1,2} \\w{3}$"))
	(visa-date (re:create-scanner
                       "^\\w{3} \\d{2}$")))
    (if (or (re:scan-to-strings bank-date line)
	    (re:scan-to-strings visa-date line))
        line nil)))
