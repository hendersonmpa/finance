;;;; taxes.lisp

(in-package #:finance)


(defparameter *federal-brackets* '((48535 0.15)
				   (48534 0.205)
				   (53404 0.26)
				   (63895 0.29)))

(defparameter *ontario-brackets* '((44740 0.0505)
				   (44742 0.0915)
				   (60518 0.1116)
				   (70000 0.1216)))

(defun apply-taxes (income bracket-list)
  (loop for (margin rate) in bracket-list
	for remaining-income = (- income margin) then (- remaining-income margin)
	for margin-amount = (if (plusp remaining-income)
				margin
				(+ margin remaining-income))
	until (minusp margin-amount)
	for tax = (* rate margin-amount)
	summing tax into total
	;; do (format t "margin: ~$ margin-amount: ~$ rate:~$ tax:~$ total:~$~%"
	;; 	   margin margin-amount rate tax total)
	finally (return total)))

(defun tax-summary (total-income taxable-income)
  (let* ((income-tax (+ (apply-taxes taxable-income *ontario-brackets*)
		       (apply-taxes taxable-income *federal-brackets*)))
	(after-tax-income (- total-income income-tax)))
  (format nil "Total income: ~$~%
Taxable income: ~$~%
Income tax: ~$~%
After tax income: ~$~%" total-income taxable-income income-tax after-tax-income)))
  



