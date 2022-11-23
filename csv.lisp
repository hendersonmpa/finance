;;; file csv.lisp
;;; Classes and Methods to parse csv bank statements
(in-package :finance)
;;(uiop:directory-files *statements*)

;;(defparameter *test-file* (concatenate 'string *statements* "csv13932.csv"))
;; (defparameter *header* (list :account :number :date :cheque :description :description2 :cad :usd))
;; (defparameter *transaction-list* (lantern::load-csv *test-file* :col-names *header*))

(defun parse-row (row)
  "Apply required transformations to a row of data"
  (destructuring-bind (&key account number date cheque description description2 cad usd) row
    (list :account account
	  :number number
	  :date (parse-date-string date)
	  :cheque cheque
	  :description description
	  :description2 description2
	  :cad cad
	  :usd usd
	  :catagory nil)))






