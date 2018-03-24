;;; file parse.lisp
;;; Classes and Methods to parse bank statements
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

(defun extract-anchors (page)
  (flet ((extract-helper (left width text)
           (let* ((l (parse-integer left))
                  (w (parse-integer width))
                  (sum (+ l w)))
             ;;(format t "~a~%" text)
             (cond ((string= text "Date") (setf *date-anchor* sum))
                   ((string= text "Description") (setf *description-anchor* l))
                   ((string= text "Withdrawals ($)") (setf *withdrawal-anchor* sum))
                   ((string= text "Deposits ($)") (setf *deposit-anchor* sum))
                   ((string= text "Balance ($)") (setf *balance-anchor* sum))
                   ;;(otherwise (print sum))
                   ))))
    (lquery:$ page
              "text"
              (combine (attr :left) (attr :width) (text))
              (map-apply #'extract-helper))
    ;; (print (list :date *date-anchor*
    ;;               :description *description-anchor*
    ;;               :withdrawal *withdrawal-anchor*
    ;;               :deposit *deposit-anchor*
    ;;               :balance *balance-anchor*))
    ;; (terpri)
    ))
;
;;(extract-anchors (elt (lquery:$ *doc* "page") 0))

;; (lquery:$ *doc*
;;           "page")

;; (lquery:$ *doc*
;;           "page"
;;           (page-anchors))

(defun datep (line)
  (let ((date-matcher (re:create-scanner
                       "^\\d{1,2} \\w{3}$")))
    (if (re:scan-to-strings date-matcher line)
        line nil)))

(defun handler-parse-number (s)
  "Convert string to number"
  (let ((no-commas (remove #\, s )))
    (handler-case (parse-number:parse-number no-commas)
      (parse-error () nil)
      (type-error () nil))))

(defun statement-filter (left width text)
  (let* ((l (parse-integer left))
         (w (parse-integer width))
         (sum (+ l w))
         (date-range (alexandria:iota 20 :start *date-anchor*))
         (description-range (alexandria:iota 10 :start *description-anchor*))
         (withdrawal-range (alexandria:iota 5 :start *withdrawal-anchor*))
         (deposit-range (alexandria:iota 10 :start *deposit-anchor*))
         ;;(balance-range (alexandria:iota 10 :start *balance-anchor*))
         (float-text (handler-parse-number text)))
    (flet ((format-text (label left width sum text)
             (format t "~a: [~a+~a=~a] ~a~%" label left width sum text)))
      (cond ((and (member sum date-range) (datep text))
             (format-text "Date" left width sum text))
            ((member l description-range) ;; used the left alignment of "Description" as anchor
             (format-text "Description" left width sum text))
            ((and (member sum withdrawal-range) float-text)
             (format-text "Withdrawal" left width sum text))
            ((and (member sum deposit-range) float-text)
             (format-text "Deposit" left width sum text))
            ;; ((and (member sum balance-range) float-text)
            ;;  (format-text "Balance" left width sum text))
            (t  nil)))))

(defun statement-filter (left width text)
  (let* ((l (parse-integer left))
         (w (parse-integer width))
         (sum (+ l w))
         (date-range (alexandria:iota 20 :start *date-anchor*))
         (description-range (alexandria:iota 10 :start *description-anchor*))
         (withdrawal-range (alexandria:iota 5 :start *withdrawal-anchor*))
         (deposit-range (alexandria:iota 10 :start *deposit-anchor*))
         ;;(balance-range (alexandria:iota 10 :start *balance-anchor*))
         (float-text (handler-parse-number text)))
    (flet ((create-plist (label text)
             (list :label label :text text)))
      (cond ((and (member sum date-range) (datep text))
             (create-plist 'date text))
            ((member l description-range) ;; used the left alignment of "Description" as anchor
             (create-plist 'description text))
            ((and (member sum withdrawal-range) float-text)
             (create-plist 'withdrawal float-text))
            ((and (member sum deposit-range) float-text)
             (create-plist 'deposit float-text))
            ;; ((and (member sum balance-range) float-text)
            ;;  (format-text "Balance" left width sum text))
            (t  nil)))))

(defun statement-all (top left width text)
  (let* ((l (parse-integer left))
         (w (parse-integer width))
         (sum (+ l w)))
    (format t "[~a|~a + ~a = ~a]~a~%" top left width sum text)))

;; (lquery:$ *doc*
;;           "text"
;;           (combine (attr :top) (attr :left) (attr :width) (text))
;;           (map-apply #'statement-all))

;; (member left '("25" "93" "105" "477" "484" "645" "796") :test #'string=)

(defun parse-statement-file (&optional (pathname (merge-pathnames *statements* "00886XXX1871-2016Dec23-2017Jan23.xml")))
  (let* ((doc (lquery:$ (initialize pathname)))
         (pages (lquery:$ doc "page"))
         (entries (loop for page across pages
                     for l = (coerce
                              (progn (extract-anchors page)
                                     (lquery:$ page "text"
                                               (combine (attr :left) (attr :width) (text))
                                               (map-apply #'statement-filter))) 'list )
                     collect l))
         (clean-entries (remove-if #'null (apply #'append entries))))
    clean-entries))


(defun gather-transactions (loe)
  "accepts parsed statement file i.e. list of entries and gathers them to output
((date type description amount)..)"
  (labels ((gather-helper (loe date accum)
             (cond ((null loe) (reverse accum))
                   ((eql (getf (first loe) :label) 'date)
                    (setf date (getf (first loe) :text))
                    (gather-helper (rest loe) date accum))
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

(defun list-to-objects (gathered-list date-range-plist)
  "accept a list of transactions, return a list of objects"
  (let ((accum nil))
    (dolist (entry gathered-list accum)
      (destructuring-bind (date type description amount) entry
        (let ((formated-date (parse-date-string date date-range-plist)))
          (case type
            (withdrawal (push (make-withdrawal formated-date description amount) accum))
            (deposit (push (make-deposit formated-date description amount) accum))
            (otherwise nil)))))))

;;(list-to-objects (gather-transactions (parse-statement-file)) (filename-date-range"00886XXX1871-2016Dec23-2017Jan23.xml"))


;; (defmethod print-object ((object transaction) stream)
;;   "print transaction object"
;;   (print-unreadable-object (object stream :type t)
;;     (with-slots (acc-t acc-n date cheq-n desc desc2 cad usd) object
;;       (format stream "~%acc:~a #:~s ~%Date:~d cheque #:~d ~%Description:~s , ~s~%cad:~$  usd:~$"
;;               acc-t acc-n date cheq-n desc desc2 cad usd))))
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
