
;;;;; Mortgage principle and payment related functions
(defun down-payment (house corp &optional (tax-rate 0.45))
  (let ((net-corp (* corp tax-rate)))
    (+ house net-corp)))


(defun dividend (cost target house &optional (tax-rate 0.45))
  "Dividend required for a mortgage < 1M"
  (loop for x from 1 to 6000000
     for  down = (down-payment house x tax-rate)
     for  principle = (- cost down)
     when (< principle target)
     do (loop-finish)
     finally (format t "Withdrawal: ~a, Down payment: ~a, Principle ~a~%" x down principle)
       (return (list x down principle))))

;; Monthly Mortgage Payment
(defmethod total-paid (principle annual-interest term &optional (frequency :monthly))
  (let ((payment (calculate-payment principle annual-interest term frequency)))
    (ecase frequency
      (:monthly
       (* payment 12 term))
      (:bi-weekly
       (* payment 26 term)))))


(defun calculate-payment (principle annual-interest term &optional (frequency :monthly))
  "payment = principal * [i(1+i)^n]/[(1+i)^n -1]"
  (flet ((annuity (principle annual-interest term yearly-payments)
           (let* ((interest (/ annual-interest yearly-payments))
                  (payments (* term yearly-payments ))
                  (compound (expt (1+ interest) payments)))
             (* principle (/ (* interest compound) (- compound 1))))))
    (let ((decimal-interest (float (/ annual-interest 100))))
      (ecase frequency (:monthly
                        (annuity principle decimal-interest term 12))
             (:bi-weekly
              (annuity principle decimal-interest term 26))))))


;; (defun net-pay-years (net-pay principle annual-interest term &optional (frequency :monthly))
;;   (let* ((payment (ecase frequency (:monthly (/ net-pay 12))
;;                          (:bi-weekly (/ net-pay 26))))
;;          (term-payment (* term payment))
;;          (total-cost ))  (let total (total-paid principle annual-interest ))
;;   ))
