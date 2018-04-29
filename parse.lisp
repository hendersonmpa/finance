;;; file parse.lisp
;;; Classes and Methods to parse pdf bank statements
(in-package :finance)
(uiop:directory-files *statements*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert pdf to xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://thomaslevine.com/!/computing/parsing-pdfs/
;; https://poppler.freedesktop.org/

;; pdftohtml -xml 451401XXXXXX1544-2016Dec29-2017Jan24.pdf

(defun pdf->xml (file-string &optional (dir-path *statements*))
  "(pdf->xml \"451401XXXXXX1544-2016Dec29-2017Jan24.pdf\") and return xml pathname"
  (let* ((pathname (merge-pathnames dir-path file-string))
	(file-path-string (namestring pathname)))
    (uiop:run-program
     (list "/usr/bin/pdftohtml" "-xml" file-path-string)
     :output t)
      (make-pathname :defaults pathname
		 :type "xml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse the xml extract using iquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<text top="668" left="67" width="24" height="12" font="1"><b>Date</b></text>

(defparameter *bank* (plump:parse (merge-pathnames *statements* "00886XXX1871-2016Dec23-2017Jan23.xml"))
 "parsed doc used for development" )

(defparameter *bank-vop* (map 'vector (lambda (page)
				   (lquery:$ page
					     "text"
					     (combine (attr :left) (attr :width) (text))))
			      (lquery:$  *bank* "page")) "Vector of pages used for development")

(defparameter *visa* (plump:parse (merge-pathnames *statements* "451401XXXXXX1544-2016Dec29-2017Jan24.xml"))
  "parsed doc used for development")

(defparameter *visa-vop* (map 'vector (lambda (page)
				   (lquery:$ page
					     "text"
					     (combine (attr :left) (text))))
			      (lquery:$  *visa* "page")) "Vector of pages used for development")

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

(defclass statement-page ()
  ((vol :initarg :vol :accessor vol :documentation "vector of lines")
   (date-anchor :initarg :date-anchor :accessor date-anchor)
   (description-anchor :initarg :description-anchor :accessor description-anchor)
   (withdrawal-anchor :initarg :withdrawal-anchor :accessor withdrawal-anchor))
  (:documentation "statement page object"))

(defclass bank-statement-page (statement-page)
  ((deposit-anchor :initarg :deposit-anchor :accessor deposit-anchor)
   (balance-anchor :initarg :balance-anchor :accessor balance-anchor))
  (:documentation "bank statement page object"))

(defclass visa-statement-page (statement-page)
  ()
  (:documentation "visa statement page object"))

(defun make-bank-statement-page (page)
  (make-instance 'bank-statement-page
		 :vol page))

(defun make-visa-statement-page (page)
  (make-instance 'visa-statement-page
		 :vol page))

(defmethod initialize-instance :after ((object bank-statement-page) &key)
  "BANK STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Withdrawals, Deposits and Balance enties in the xml file,
these are then used to determine which category to assign to the the
rest of the entries"
  (with-accessors ((vol vol)
		   (date-anchor date-anchor)
		   (description-anchor description-anchor)
		   (withdrawal-anchor withdrawal-anchor)
		   (deposit-anchor deposit-anchor)
		   (balance-anchor balance-anchor)) object
    (flet ((get-anchors (line)
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
	(map 'nil #'get-anchors (vol object)))))

(defmethod initialize-instance :after ((object visa-statement-page) &key)
    "VISA STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Number, Withdrawals, Deposits and Balance enties in the xml file,
these are then used to determine which category to assign to the
rest of the entries"
    (with-accessors ((vol vol)
		     (date-anchor date-anchor)
		     (description-anchor description-anchor)
		     (withdrawal-anchor withdrawal-anchor)) object
      (flet ((get-anchors (line)
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
	(map 'nil #'get-anchors (vol object)))))

(defun extract-pages (parsed-content)
  "accept plump:parse output, return a vector of pages composed of text and attributes"
  (map 'vector (lambda (page)
		 (lquery:$ page "text"
			   (combine (attr :left) (attr :width) (text))))
       (lquery:$ parsed-content "page")))

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

(defmethod filter-page ((object bank-statement-page))
  "BANK STATEMENT PAGE:compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest"
  (with-accessors ((vol vol)
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
			 ((and (member sum withdrawal-range) float-text)
			  (create-plist 'withdrawal float-text))
			 ((and (member sum deposit-range) float-text)
			  (create-plist 'deposit float-text))
			 (t nil))))))
      (remove-if #'null (map 'list #'filter-helper (vol object))))))

(defmethod filter-page ((object visa-statement-page))
  "VISA STATEMENT:compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest"
  (with-accessors ((vol vol)
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
			    (create-plist 'withdrawal (dollars-parse text)))
			   (t  nil))))))
	(remove-if #'null (map 'list #'filter-helper (vol object)))))))


;; TODO: think about combining filter-page and get-anchors into the intialize-instance :after method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transaction classes and methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun list-to-objects (transaction-list date-range-plist)
  "accept a list of transactions, return a list of objects"
  (let ((accum nil))
    (dolist (entry transaction-list accum)
      (destructuring-bind (date type description amount) entry
        (let ((formated-date (parse-date-string date date-range-plist)))
          (case type
            (withdrawal (push (make-withdrawal formated-date description amount) accum))
            (deposit (push (make-deposit formated-date description amount) accum))
            (otherwise nil)))))))

(defun bank-statement-transactions (loe)
  "accepts parsed statement file i.e. list of entries and gathers them to output
plist date type description amount.."
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
    (gather-helper loe nil nil)))

(defun visa-statement-transactions (loe)
  "accepts parsed statement file i.e. list of entries and gathers them to output
plist date type description amount.."
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
    (gather-helper loe nil nil)))


;;(list-to-objects (gather-transactions (parse-statement-file)) (filename-date-range"00886XXX1871-2016Dec23-2017Jan23.xml"))

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

;; (dolist (ob (list-to-objects (bank-statement-transactions (bank-statement-parse)) (filename-date-range"00886XXX1871-2016Dec23-2017Jan23.xml")))
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



