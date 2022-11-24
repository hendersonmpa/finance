;;; file pdf.lisp
;;; Classes and Methods to parse pdf bank statements
(in-package :finance)
(uiop:directory-files *statements-dir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert pdf to xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://thomaslevine.com/!/computing/parsing-pdfs/
;; https://poppler.freedesktop.org/

;; pdftohtml -xml 451401XXXXXX1544-2016Dec29-2017Jan24.pdf

(defun pdf->xml (file-string &optional (dir-path *statements-dir*))
  "(pdf->xml \"451401XXXXXX1544-2016Dec29-2017Jan24.pdf\") and return xml pathname"
  (let* ((pathname (merge-pathnames dir-path file-string))
	(file-path-string (namestring pathname)))
    (uiop:run-program
     (list "/usr/bin/pdftohtml" "-xml" file-path-string)
     :output t)
      (make-pathname :defaults pathname
		 :type "xml")))

;; (pdf->xml "Chequing Statement-1871 2022-10-21.pdf")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse the xml extract using iquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<text top="668" left="67" width="24" height="12" font="1"><b>Date</b></text>

(defparameter *bank* (plump:parse (merge-pathnames *statements-dir* "00886XXX1871-2016Dec23-2017Jan23.xml"))
 "parsed doc used for development" )

(defparameter *bank-vop* (map 'vector (lambda (page)
				   (lquery:$ page
					     "text"
					     (combine (attr :left) (attr :width) (text))))
			      (lquery:$  *bank* "page"))
  "Vector of pages used for development")

(defparameter *visa* (plump:parse (merge-pathnames *statements-dir* "451401XXXXXX1544-2016Dec29-2017Jan24.xml"))
  "parsed doc used for development")

(defparameter *visa-vop* (map 'vector (lambda (page)
				   (lquery:$ page
					     "text"
					     (combine (attr :left) (text))))
			      (lquery:$  *visa* "page"))
  "Vector of pages used for development")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Statement classes 
(defclass statement ()
  ((pages :initarg :pages :accessor pages)
   (transactions :initarg :transactions :accessor transactions))
  (:documentation "parent statement object"))

(defclass visa-statement (statement)
  ()
  (:documentation "visa statement object"))

(defclass bank-statement (statement)
  ()
  (:documentation "bank statement object"))

;;; Page classes  

(defclass statement-page ()
  ((raw :initarg :raw :accessor raw :documentation "raw data")
   (filtered :initarg :filtered :accessor filtered :documentation "filtered lines")
   (transactions :initarg :transactions :accessor transactions :documentation "list of transactions")
   (date-anchor :initarg :date-anchor :initform 0 :accessor date-anchor)
   (description-anchor :initarg :description-anchor :initform 0 :accessor description-anchor)
   (withdrawal-anchor :initarg :withdrawal-anchor :initform 0 :accessor withdrawal-anchor))
  (:documentation "statement page object"))

(defclass bank-statement-page (statement-page)
  ((deposit-anchor :initarg :deposit-anchor :initform 0 :accessor deposit-anchor)
   (balance-anchor :initarg :balance-anchor :initform 0 :accessor balance-anchor))
  (:documentation "bank statement page object"))

(defclass visa-statement-page (statement-page)
  ()
  (:documentation "visa statement page object"))

;;; Transaction classes

(defclass transaction ()
  ((date :initarg :date :accessor date)
   (description :initarg :description :accessor description)
   (amount :initarg :amount :accessor amount))
  (:documentation "transaction class"))

(defclass deposit (transaction)
  ()
  (:documentation "deposit class"))

(defclass withdrawal (transaction)
  ()
  (:documentation "withdrawal class"))

(defclass visa (transaction)
  ()
  (:documentation "visa class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialising Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; statements
(defun make-bank-statement (pdf-file-string)
  "Accept a bank statement pdf file string and return a bank-statement object"
  (let* ((xml-file-name (pdf->xml pdf-file-string))
	 (parsed-content (plump:parse xml-file-name))
	 (raw-pages (extract-pages parsed-content)))
    (make-instance 'bank-statement
		   :pages (map 'vector #'make-bank-statement-page raw-pages))))
 

(defun make-visa-statement (pdf-file-string)
  "Accept a visa statement pdf file string and return a visa-statement object"
  (let* ((xml-file-name (pdf->xml pdf-file-string))
	 (parsed-content (plump:parse xml-file-name))
	 (raw-pages (extract-pages parsed-content)))
    (make-instance 'visa-statement
		   :pages (map 'vector #'make-visa-statement-page raw-pages))))


;;; pages
(defun make-bank-statement-page (page)
  (make-instance 'bank-statement-page
		 :raw page))

(defun make-visa-statement-page (page)
  (make-instance 'visa-statement-page
		 :raw page))
;;; transactions
(defun make-deposit (date description amount)
  (make-instance 'deposit
                 :date date
                 :description description
                 :amount amount))

(defun make-withdrawal (date description amount)
  (make-instance 'withdrawal
                 :date date
                 :description description
                 :amount amount))

(defun make-visa (date description amount)
  (make-instance 'visa
                 :date date
                 :description description
                 :amount amount))

(defmethod initialize-instance :after ((object statement-page) &key)
  "at this point only raw-pages are added, these are now parsed to get anchors, filter lines using anchors, and gather into transactions"
  (progn
    (get-anchors object)
    (filter-page object)
    (gather-transactions object)))

(defmethod initialize-instance :after ((object statement) &key)
  "at this point only raw-pages are added, these are now parsed to get anchors, filter lines using anchors, and gather into transactions"
  (with-accessors ((transactions transactions)) object
      (setf transactions (map 'vector #'make-transaction-objects
			      (slot-value object 'pages)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-pages (parsed-content)
  "accept plump:parse output, return a vector of pages composed of text and attributes"
  (map 'vector (lambda (page)
		 (lquery:$ page "text"
			   (combine (attr :left) (attr :width) (text))))
       (lquery:$ parsed-content "page")))

(defmethod get-anchors ((object bank-statement-page))
  "BANK STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Withdrawals, Deposits and Balance enties in the xml file,
these are then used to determine which category to assign to the the
rest of the entries"
  (with-accessors ((raw raw)
		   (date-anchor date-anchor)
		   (description-anchor description-anchor)
		   (withdrawal-anchor withdrawal-anchor)
		   (deposit-anchor deposit-anchor)
		   (balance-anchor balance-anchor)) object
    (flet ((get-anchors-helper (line)
	     (destructuring-bind (left width text) line
	       (let* ((left-num (parse-integer left))
		     (width-num (parse-integer width))
		     (sum (+ left-num width-num))) ;; sum is used for righ justified columns
		 ;;(format t "~a~%" text)
		 (cond ((string= text "Date") (setf date-anchor left-num)) ;; left justified
		       ((string= text "Description") (setf description-anchor left-num)) ;; left justified
		       ((string= text "Withdrawals ($)") (setf withdrawal-anchor sum))  ;; right justified
		       ((string= text "Deposits ($)") (setf deposit-anchor sum)) ;; right justified
		       ((string= text "Balance ($)") (setf balance-anchor sum)) ;; right justified
		       (t nil))))))
      (map 'nil #'get-anchors-helper (raw object)))))


(defmethod get-anchors ((object visa-statement-page))
    "VISA STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Number, Withdrawals, Deposits and Balance entries in the xml file,
these are then used to determine which category to assign to the
rest of the entries"
    (with-accessors ((raw raw)
		     (date-anchor date-anchor)
		     (description-anchor description-anchor)
		     (withdrawal-anchor withdrawal-anchor)) object
      (flet ((get-anchors-helper (line)
	       (destructuring-bind (left width text) line 
		 (let* ((left-num (parse-integer left))
			(width-num (parse-integer width))
			(sum (+ left-num width-num))) ;; sum is used for righ justified columns
		     (cond ((string= text "TRANSACTION POSTING")
			    (setf date-anchor left-num
				  description-anchor (+ left-num 100 ))) ;; left justified
			   ((string= text "AMOUNT ($)")
			    (setf withdrawal-anchor sum))  ;; right justified
			   (t nil))))))
	(map 'nil #'get-anchors-helper (raw object)))))


(defmethod filter-page ((object bank-statement-page))
  "BANK STATEMENT PAGE: Uses the contents of the RAW slot. Compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest. Puts them in the FILTERED slot"
  (with-accessors ((raw raw)
		   (filtered filtered)
		   (date-anchor date-anchor)
		   (description-anchor description-anchor)
		   (withdrawal-anchor withdrawal-anchor)
		   (deposit-anchor deposit-anchor)
		   (balance-anchor balance-anchor)) object
    (labels ((create-plist (label text)
	       (list :label label :text text))
	     (filter-helper (line)
	       (destructuring-bind (left width text) line 
		 (let* ((left-num (parse-integer left))
			(width-num (parse-integer width))
			(sum (+ left-num width-num))
			(withdrawal-range (alexandria:iota 5 :start withdrawal-anchor))
			(deposit-range (alexandria:iota 10 :start deposit-anchor))
			(float-text (handler-parse-number text)))
		   (cond ((and (equalp left-num date-anchor) (datep text)) ;; used the left alignment of "Date" as anchor
			  (create-plist 'date text))
			 ((equal left-num description-anchor) ;; used the left alignment of "Description" as anchor
			  (create-plist 'description text))
			 ;; ((equal left-num (+ description-anchor 12)) ;; used the left alignment of "Description" + 12 as anchor
			 ;;  (create-plist 'description-sub text)) ;; leave this out for now
			 ((and (member sum withdrawal-range) float-text)
			  (create-plist 'withdrawal float-text))
			 ((and (member sum deposit-range) float-text)
			  (create-plist 'deposit float-text))
			 (t nil))))))
      (setf filtered (remove-if #'null (map 'list #'filter-helper (raw object)))))))

(defmethod filter-page ((object visa-statement-page))
  "VISA STATEMENT:compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest"
  (with-accessors ((raw raw)
		   (filtered filtered)
		   (date-anchor date-anchor)
		   (description-anchor description-anchor)
		   (withdrawal-anchor withdrawal-anchor)) object
    (let ((previous-label nil))
      (labels ((create-plist (label text)
		 (list :label label :text text))
	       (filter-helper (line)
		 (destructuring-bind (left width text) line 
		   (let* ((left-num (parse-integer left))
			  (width-num (parse-integer width))
			  (sum (+ left-num width-num))
			  (description-range (alexandria:iota 20 :start description-anchor))
			  (withdrawal-range (alexandria:iota 15 :start (- withdrawal-anchor 10))))
		     (cond ((and (equalp left-num date-anchor) (datep text)) ;; used the left alignment of "Date" as anchor
			    (setf previous-label 'date)
			    (create-plist 'date text))
			   ((and (member left-num description-range) (not (eql previous-label 'description)))
			    ;;left alignment of "Description" as anchor and previous line not a descriptione
			    (setf previous-label 'description)
			    (create-plist 'description text))
			   ((member sum withdrawal-range)
			    (setf previous-label 'withdrawal)
			    (create-plist 'visa (dollars-parse text)))
			   (t  nil))))))
	(setf filtered (remove-if #'null (map 'list #'filter-helper (raw object))))))))

(defmethod gather-transactions ((object bank-statement-page))
  "Uses the contents of the FILTERED i.e. filtered list of entries and gathers them to output plist date type description amount.."
  (with-accessors ((filtered filtered)
		   (transactions transactions)) object
      (labels ((gather-helper (loe date accum)
		 (cond ((null loe) (reverse accum))
		       ((eql (getf (first loe) :label) 'date) ;; if the entry is a date hold onto that date until the next date entry
			(setf date (getf (first loe) :text)) 
			(gather-helper (rest loe) date accum))
		       ((null date)
			(gather-helper (rest loe) date accum)) ;; if a date has not been found move to the next entry
		       (t (let ((label (getf (first loe) :label)))
			    (cond ((eql label 'description)
				   (push (list date
					       (getf (second loe) :label)
					       (getf (first loe) :text)
					       (getf (second loe) :text)) accum)
				   (gather-helper (rest loe) date accum))
				  (t (gather-helper (rest loe) date accum))))))))
	(setf transactions (gather-helper filtered nil nil)))))

(defmethod gather-transactions ((object visa-statement-page))
  "accepts parsed statement file i.e. filtered list of entries and gathers them to output
plist date type description amount.."
  (with-accessors ((filtered filtered)
		   (transactions transactions)) object
    (labels ((gather-helper (loe date accum)
	       (cond ((null loe) (reverse accum))
		     ((eql (getf (first loe) :label) 'date) ;; if the entry is a date hold onto that date until the next date entry
		      (setf date (getf (first loe) :text)) 
		      (gather-helper (rest loe) date accum))
		     ((null date)
		      (gather-helper (rest loe) date accum)) ;; if a date has not been found move to the next entry
		     (t (let ((label (getf (first loe) :label)))
			  (cond ((eql label 'description)
				 (push (list date
					     (getf (second loe) :label)
					     (getf (first loe) :text)
					     (getf (second loe) :text)) accum)
				 (gather-helper (rest loe) date accum))
				(t (gather-helper (rest loe) date accum))))))))
      (setf transactions (gather-helper filtered nil nil)))))


;;;; Get the date range of the transaction
;; use this line in the RAW slot of PAGE 1 to get date range
;; BANK ("615" "275" "From September 23, 2022 to October 21, 2022")
;; VISA ("90" "248" "STATEMENT FROM OCT 18 TO NOV 17, 2022")

(defmethod statement-date-range ((object bank-statement))
  "accepts a filename form '00886XXX1871-2016Dec23-2017Jan23.xml'"
  (re:register-groups-bind (start-year start-month end-year end-month)
      ("\\w{12}-(\\d{4})(\\w{3})\\d{1,2}-(\\d{4})(\\w{3})\\d{1,2}" filename)
    (list :start-month start-month :start-year start-year :end-month end-month :end-year end-year)))


(defgeneric make-transaction-objects (statement-page)
  (:documentation "accept a list of statement-page objects, and returns a list of objects used in the 'INTIALIZE-INSTANCE :AFTER' method"))

(defmethod make-transaction-objects ((object bank-statement-page))
    (with-accessors ((transactions transactions)) object
      (let ((accum nil)
	    (year "2022") ;; place holder until it is extracted from the statement
	    ;; TODO figure out how to deal with statements that span two years.
	      ;;(date-range-plist (filename-date-range statement-file-name))
	      )
	  (dolist (entry transactions accum)
	    (destructuring-bind (date type description amount) entry
	      (let ((formated-date (parse-bank-date-string date year)))
		(case type
		  (withdrawal (push (make-withdrawal formated-date description amount) accum))
		  (deposit (push (make-deposit formated-date description amount) accum))
		  (visa (push (make-visa formated-date description amount) accum))
		  (otherwise nil))))))))

(defmethod make-transaction-objects ((object visa-statement-page))
    (with-accessors ((transactions transactions)) object
      (let ((accum nil)
	    (year "2022") ;; place holder until it is extracted from the statement
	    ;; TODO figure out how to deal with statements that span two years.
	      ;;(date-range-plist (filename-date-range statement-file-name))
	      )
	  (dolist (entry transactions accum)
	    (destructuring-bind (date type description amount) entry
	      (let ((formated-date (parse-visa-date-string date year)))
		(case type
		  (withdrawal (push (make-withdrawal formated-date description amount) accum))
		  (deposit (push (make-deposit formated-date description amount) accum))
		  (visa (push (make-visa formated-date description amount) accum))
		  (otherwise nil))))))))

(defparameter *bank-tester* (make-bank-statement "Chequing Statement-1871 2022-10-21.pdf"))
(defparameter *visa-tester* (make-visa-statement "Visa Statement-5953 2022-11-17.pdf"))


(map 'vector #'make-bank-statement-page raw-pages)



(defmethod print-object ((object transaction) stream)
  "print transaction object"
  (print-unreadable-object (object stream :type t)
    (with-slots (date description amount) object
      (format stream "~%Date:~d, Description:~s, Amount:~$"
              date description amount))))

(defmethod print-object ((object deposit) stream)
  "print transaction object"
  (print-unreadable-object (object stream :type t)
    (with-slots (date description amount) object
      (format stream "~%Date:~d, Description:~s, Deposit:~$"
              date description amount))))

(defmethod print-object ((object withdrawal) stream)
  "print transaction object"
  (print-unreadable-object (object stream :type t)
    (with-slots (date description amount) object
      (format stream "~%Date:~d, Description:~s, Withdrawal:~$"
              date description amount))))

(defmethod format-object ((object deposit) stream)
  "print transaction object: date, description, withdrawal, deposit"
  (with-slots (date description amount) object
    (format stream "~d, ~s, ,~$~%" 
	    date description amount)))

(defmethod format-object ((object withdrawal) stream)
  "print transaction object: date, description, withdrawal, deposit"
  (with-slots (date description amount) object
    (format stream "~d, ~s, ~$, ~%"
	    date description amount)))

;; (dolist (ob (list-to-objects (bank-statement-transactions (bank-statement-parse)) (filename-date-range "00886XXX1871-2016Dec23-2017Jan23.xml")))
;;   (format-object ob t))

;; (defun parse-transaction-csv (pathname)
;;   (cl-csv:read-csv pathname
;;                    :map-fn #'make-transaction
;;                    :skip-first-p t
;;                    :separator #\,
;;                                         ;:quote t ;; there are quotes in comment strings
;;                    :unquoted-empty-string-is-nil t))

(defun archive-file (old-pathname &optional (to-subdir "archive"))
  "Archive csv file"
  (let ((new-pathname
         (make-pathname :defaults old-pathname
                        :directory (append
                                    (butlast (pathname-directory old-pathname))
                                    (list to-subdir)))))
    (rename-file old-pathname new-pathname)))

(defun load-dir (&optional (dir-name *statements*))
  "Upload files to db table based on regex match"
  (let ((path-list (uiop:directory-files dir-name))
        (accum nil))
    (dolist (pathname path-list (apply #'nconc accum))
      (push (parse-transaction-csv pathname) accum))))

;;(defparameter *lot* (load-dir))

(defun filter-desc (str data)
  (flet ((pred (e)
           (search str e :test #'string-equal)))
    (remove-if-not #'pred data :key #'desc)))

;;(filter-desc "netflix" *lot*)

;; filter date

(defun payee-sum (payee lot)
  (let ((fl (filter (lambda (e) (search )))))))



