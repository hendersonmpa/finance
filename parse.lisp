;;; file parse.lisp
;;; Classes and Methods to parse pdf bank statements
(in-package :finance)

(defparameter *date-anchor* nil)
(defparameter *description-anchor* nil)
(defparameter *withdrawal-anchor* nil)
(defparameter *deposit-anchor* nil)
(defparameter *balance-anchor* nil)
(uiop:directory-files *statements*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert pdf to xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://thomaslevine.com/!/computing/parsing-pdfs/
;; https://poppler.freedesktop.org/

;; pdftohtml -xml 451401XXXXXX1544-2016Dec29-2017Jan24.pdf

(defun pdf->xml (input-file-string)
  "pdftohtml -xml filename.pdf"
  ;; (pdf->xml "451401XXXXXX1544-2016Dec29-2017Jan24.pdf")
  (uiop:run-program
   (list "/usr/bin/pdftohtml" "-xml" input-file-string)
   :output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse the xml extract using iquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<text top="668" left="67" width="24" height="12" font="1"><b>Date</b></text>

(defparameter *doc* (lquery:$ (initialize (merge-pathnames *statements* "00886XXX1871-2016Dec23-2017Jan23.xml"))))

(defun bank-statement-get-anchors (page)
  "BANK STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Withdrawals, Deposits and Balance enties in the xml file,
these are then used to determine which category to assign to the the
rest of the entries"
  (flet ((extract-helper (left width text)
           (let* ((l (parse-integer left))
                  (w (parse-integer width))
                  (sum (+ l w))) ;; sum is used for righ justified columns
             ;;(format t "~a~%" text)
             (cond ((string= text "Date") (setf *date-anchor* l)) ;; right justified
                   ((string= text "Description") (setf *description-anchor* l)) ;; right justified
                   ((string= text "Withdrawals ($)") (setf *withdrawal-anchor* sum))  ;; left justified
                   ((string= text "Deposits ($)") (setf *deposit-anchor* sum)) ;; left justified
                   ((string= text "Balance ($)") (setf *balance-anchor* sum)) ;; left justified
                   ;;(otherwise (print sum))
                   ))))
    (lquery:$ page
              "text"
              (combine (attr :left) (attr :width) (text))
              (map-apply #'extract-helper))))

(defun visa-statement-get-anchors (page)
  "VISA STATEMENT: extracts LEFT and WIDTH column anchor values for the Date,
Description, Number, Withdrawals, Deposits and Balance enties in the xml file,
these are then used to determine which category to assign to the the
rest of the entries"
  (flet ((extract-helper (left text)
           (let* ((l (parse-integer left))
		  ;;                (w (parse-integer width))
		  ;;                  (sum (+ l w))
		  ) ;; sum is used for righ justified columns
             ;;(format t "~a~%" text)
             (cond ((string= text "TRANSACTION POSTING")
		    (setf *date-anchor* l
			  *description-anchor* (+ l 100 ))) ;; left justified
                   ((string= text "AMOUNT ($)")
		    (setf *withdrawal-anchor* l))  ;; right justified
		   (t nil)))))
    (lquery:$ page
              "text"
              (combine (attr :left) (text))
              (map-apply #'extract-helper))))

(defun handler-parse-number (s)
  "Convert string to number"
  (let ((no-commas (remove #\, s )))
    (handler-case (parse-number:parse-number no-commas)
      (parse-error () nil)
      (type-error () nil))))

(defun datep (line)
  "date predicate"
  (let ((bank-date (re:create-scanner
		    "^\\d{1,2} \\w{3}$"))
	(visa-date (re:create-scanner
                       "^\\w{3} \\d{2}$")))
    (if (or (re:scan-to-strings bank-date line)
	    (re:scan-to-strings visa-date line))
        line nil)))

(defun dollars-parse (line)
  "date predicate"
  (let* ((dollars (re:create-scanner
		   "^\\$(\\d*\\.\\d{2})$")))
    (re:register-groups-bind (digits-only)
			  (dollars line)
			(handler-parse-number digits-only))))

(defun bank-statement-filter (left width text)
  "BANK STATEMENT:compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest"
  (let* ((l (parse-integer left))
         (w (parse-integer width))
         (sum (+ l w))
         (withdrawal-range (alexandria:iota 5 :start *withdrawal-anchor*))
         (deposit-range (alexandria:iota 10 :start *deposit-anchor*))
         (float-text (handler-parse-number text)))
    (flet ((create-plist (label text)
             (list :label label :text text)))
      (cond ((and (equalp l *date-anchor*) (datep text)) ;; used the left alignment of "Date" as anchor
             (create-plist 'date text))
            ((equal l *description-anchor*) ;; used the left alignment of "Description" as anchor
             (create-plist 'description text))
            ((and (member sum withdrawal-range) float-text)
             (create-plist 'withdrawal float-text))
            ((and (member sum deposit-range) float-text)
             (create-plist 'deposit float-text))
	    (t  nil)))))

(defun visa-statement-filter (left text)
  "VISA STATEMENT:compares the LEFT or (SUM LEFT WIDTH) to the ANCHOR parameters to identify and collect entries of interest"
  (let* ((l (parse-integer left))
;;         (w (parse-integer width))
;;         (sum (+ l w))
         (description-range (alexandria:iota 20 :start *description-anchor*))
         (withdrawal-range (alexandria:iota 15 :start *withdrawal-anchor*))
;;         (float-text (handler-parse-number text))
	 )
    (flet ((create-plist (label left text)
             (list :label label :left left :text text)))
      (cond ((and (equalp l *date-anchor*) (datep text)) ;; used the left alignment of "Date" as anchor
             (create-plist 'date left text))
            ((member l description-range);; used the left alignment of "Description" as anchor
             (create-plist 'description left text))
            ((and (member l withdrawal-range) (dollars-parse text))
             (create-plist 'withdrawal left (dollars-parse text)))
            (t  nil)))))

(defun bank-statement-parse (&optional (pathname (merge-pathnames *statements* "00886XXX1871-2016Dec23-2017Jan23.xml")))
  (let* ((doc (lquery:$ (initialize pathname)))
         (pages (lquery:$ doc "page"))
         (entries (loop for page across pages
                     for l = (coerce
                              (progn (bank-statement-get-anchors page)
                                     (lquery:$ page "text"
                                               (combine (attr :left) (attr :width) (text))
                                               (map-apply #'bank-statement-filter))) 'list )
                     collect l))
         (clean-entries (remove-if #'null (apply #'append entries))))
    clean-entries))


(defun visa-statement-parse (&optional (pathname (merge-pathnames *statements* "451401XXXXXX1544-2016Dec29-2017Jan24.xml")))
  (let* ((doc (lquery:$ (initialize pathname)))
         (pages (lquery:$ doc "page"))
         (entries (loop for page across pages
                     for l = (coerce
                              (progn (visa-statement-get-anchors page)
                                     (lquery:$ page "text"
                                               (combine (attr :left) (text))
                                               (map-apply #'visa-statement-filter))) 'list )
                     collect l))
         (clean-entries (remove-if #'null (apply #'append entries))))
    clean-entries))

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
                   (t
                    (let ((label (getf (first loe) :label)))
                      (cond ((eql label 'description)
                             (push (list date
                                         (getf (second loe) :label)
                                         (getf (first loe) :text)
                                         (getf (second loe) :text)) accum)
                             (gather-helper (rest loe) date accum))
                            (t (gather-helper (rest loe) date accum))))))))
    (gather-helper loe nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transaction classes and methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transaction ()
  ((date
    :initarg :date
    :type clsql:date
    :accessor date)
   (description
    :initarg :description
    :type string
    :accessor description)
   (amount
    :initarg :amount
    :type float
    :accessor amount))
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



