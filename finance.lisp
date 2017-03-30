;;;; finance.lisp

(in-package #:finance)

;;; "finance" goes here. Hacks and glory await!

;; Monthly Mortgage Payment
(defun calculate-payment (principle annual-interest term &optional (frequency :monthly))
  "M = P[i(1+i)^n]/[(1+i)^n -1]"
  (flet ((annuity (principle annual-interest term yearly-payments)
           (let* ((interest (/ annual-interest yearly-payments))
                  (payments (* term yearly-payments ))
                  (compound (expt (1+ interest) payments)))
             (* principle (/ (* interest compound) (- compound 1))))))
    (ecase frequency (:monthly
                      (annuity principle annual-interest term 12))
           (:bi-weekly
            (annuity principle annual-interest term 26)))))

(defun total-paid (principle annual-interest term &optional (frequency :monthly))
  (let ((payment (calculate-payment principle annual-interest term frequency)))
    (ecase frequency (:monthly
                      (* payment 12 term))
           (:bi-weekly
            (* payment 26 term)))))
