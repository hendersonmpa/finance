;;;; finance.lisp

(in-package #:finance)

;;;;;;;; Debt
(defclass debt ()
  ((label :initarg :label :accessor label)
   (original-balance :initarg :original-balance :accessor original-balance)
   (principle :initarg :principle :accessor principle)
   (interest-rate :initarg :interest-rate :accessor interest-rate)
   (interest-type :initarg :interest-type :accessor interest-type))
    (:documentation "debt superclass"))

;;; Mortgage
(defclass mortgage (debt)
  ((original-amortization :initarg :original-amortization :accessor original-amortization)
   (remaining-amortization :initarg :remaining-amortization :accessor remaining-amortization)
   (term :initarg :term :accessor term)
   (payment-frequency :initarg :payment-frequency :accessor payment-frequency)
   (payment :initarg :payment :accessor payment))
  (:documentation "mortgage class"))

(defun make-mortgage (label original-balance principle interest-rate interest-type original-amortization remaining-amortization term payment-frequency payment)
  (make-instance 'mortgage
		 :label label
		 :original-balance original-balance
		 :principle principle
		 :interest-rate (/ interest-rate 100)
		 :interest-type interest-type
		 :original-amortization original-amortization
		 :remaining-amortization remaining-amortization
		 :term term 
		 :payment-frequency payment-frequency
		 :payment payment))


(defgeneric print-mortgage (mortgage)
  (:documentation "Prints the mortgage  object slots"))

(defmethod print-mortgage ((object mortgage))
  (with-accessors ((label label)
		   (balance original-balance)
		   (principle principle)
		   (o-amortization original-amortization)
		   (r-amortization remaining-amortization)
		   (rate interest-rate)
		   (type interest-type)) object
    (format nil "Name: ~a ~%
Original Balance: ~a ~%
Remaining Principle: ~a ~%
Original Amortization: ~a ~%
Remaining Amortization: ~a ~%
Interest Rate: ~a ~%
Type: ~a ~%" label balance principle o-amortization r-amortization rate type)))


;;; Line of Credit
(defclass credit-line (debt)
  ()
  (:documentation "credit line class"))

(defun make-credit-line (label original-balance principle interest-rate interest-type)
  (make-instance 'credit-line
		 :label label
		 :original-balance original-balance
		 :principle principle
		 :interest-rate (/ interest-rate 100)
		 :interest-type interest-type))


;;;;;; Assets
(defclass asset ()
  ((label :initarg :label :accessor label)
   (value :initarg :value :accessor value))
  (:documentation "asset superclass"))

(defclass investment (asset)
  ((book-cost :initarg :book-cost :accessor book-cost)
   (twrr :initarg :twrr :accessor twrr)) ;;time weighted rate of return
  (:documentation "investment class"))

(defclass property (asset)
  ((book-cost :initarg :book-cost :accessor book-cost))
  (:documentation "property class"))

(defun make-investment (label value book-cost twrr)
  (make-instance 'investment
		 :label label
		 :value value
		 :book-cost book-cost
		 :twrr twrr))

(defun make-property (label value book-cost)
  (make-instance 'property
		 :label label
		 :value value
		 :book-cost book-cost))

(defmethod get-balance ((object debt))
  (* -1 (principle object)))

(defmethod get-balance ((object asset))
  (value object))


