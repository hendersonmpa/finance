;;;; debt.lisp

(in-package #:finance)

;; Monthly Mortgage Payment
(defun total-paid (principle annual-interest term &optional (frequency :monthly))
  (let ((payment (calculate-payment principle annual-interest term frequency)))
    (ecase frequency (:monthly
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

(defun total-paid (principle annual-interest term &optional (frequency :monthly))
  "return total paid and total interest"
  (let ((payment (calculate-payment principle annual-interest term frequency)))
    (flet ((total (payment payments term)
             (let ((tot (* payment payments term)))
               (values tot (- tot principle) payment))))
      (ecase frequency (:monthly
                        (total payment 12 term))
             (:bi-weekly
              (total payment 26 term))))))

;; (defun net-pay-years (net-pay principle annual-interest term &optional (frequency :monthly))
;;   (let* ((payment (ecase frequency (:monthly (/ net-pay 12))
;;                          (:bi-weekly (/ net-pay 26))))
;;          (term-payment (* term payment))
;;          (total-cost ))  (let total (total-paid principle annual-interest ))
;;   ))

;; (defun ammortization (principle payment annual-interest)
;;   (loop while (>= principle payment)
;;      with interest-payment = (* principle annual-interest)
;;      do (setf principle (- principle (- payment interest-payment)))
;;      do (print principle)
;;      count t into months
;;      finally (return (1+ months))))

(defun amortization (principle payment annual-interest &optional (frequency :monthly) (print nil))
  (let ((interest-rate (ecase frequency (:monthly (/ annual-interest 12))
			      (:bi-weekly
			       (/ annual-interest 26))))
	(divisor (ecase frequency (:monthly 12)
			(:bi-weekly 26)))
	(accum nil))
    (do* ((count 0 (1+ count))
	  (total 0 (* payment count))
	  (years 0 (float (/ count divisor)))
	  (interest-payment (* principle interest-rate) (* remaining-principle interest-rate))
	  (principle-payment (- payment interest-payment) (- payment interest-payment))
	  (remaining-principle principle (- remaining-principle principle-payment)))
	 ((or (< remaining-principle principle-payment))
	  (if print
	      (format t "payments ~d, years ~d, total ~d, principle ~d, interest-payment ~d, principle-payment ~d~%" count years total remaining-principle interest-payment principle-payment)
	      (nreverse accum)))

	  ;;    (format t "payments ~d, years ~d, total ~d, principle ~d, interest-payment ~d, principle-payment ~d~%" count years total remaining-principle interest-payment principle-payment)
      (push (list count years total remaining-principle interest-payment principle-payment) accum))))

(defun amortization-plot (dataset)
  "Plot an of an amortization"
  (let ((file-name (merge-pathnames *figures-dir* "amortization.pdf")))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name
			     :terminal '(pdfcairo enhanced font "Verdana,10")
			     :key '(on)
			     ;;:logscale '(y)
			     :title "Amortization"
			     :xlabel "years"
			     :ylabel "amount"
			     :ytic '(nomirror font ",8")
			     :xtic '(nomirror font ",8" rotate by -45)
			     :tic '(nomirror out scale 0.75)
			     :border '(11 front linecolor rgb ("'#808080'") lt 1)
			     :style '(fill solid 0.5 noborder)
			     :style '(line 1 linetype 2 linewidth 2 linecolor rgb ("'#8F5E99'")))
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (row dataset)
	   ;; count years total remaining-principle interest-payment principle-payment
	   (format t "~&~{~d ~d ~d ~d ~d ~d~%~}" row)))
       :using '("2:3") :with '(lines) :title "total")
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (row dataset)
	   ;; count years total remaining-principle interest-payment principle-payment
	   (format t "~&~{~d ~d ~d ~d ~d ~d~%~}" row)))
       :using '("2:4 w lines title 'principle'"))
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (row dataset)
	   ;; count years total remaining-principle interest-payment principle-payment
	   (format t "~&~{~d ~d ~d ~d ~d ~d~%~}" row)))
       :using '("2:5 w lines"))
            (eazy-gnuplot:plot
       (lambda ()
	 (dolist (row dataset)
	   ;; count years total remaining-principle interest-payment principle-payment
	   (format t "~&~{~d ~d ~d ~d ~d ~d~%~}" row)))
       :using '("2:6 w lines"))) file-name))


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
