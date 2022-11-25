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


;; (defun load-dir (&optional (dir-name *statements-dir*))
;;   "Upload files to db table based on regex match"
;;   (let ((path-list (uiop:directory-files dir-name))
;;         (accum nil))
;;     (dolist (pathname path-list (apply #'nconc accum))
;;       (push (parse-transaction-csv pathname) accum))))

;;(defparameter *lot* (load-dir))

;; (defun filter-desc (str data)
;;   (flet ((pred (e)
;;            (search str e :test #'string-equal)))
;;     (remove-if-not #'pred data :key #'desc)))

;;(filter-desc "netflix" *lot*)

;; filter date

;; (defun payee-sum (payee lot)
;;   (let ((fl (filter (lambda (e) (search )))))))


