;;;; debt.lisp

(in-package #:finance)

(defmethod amortization ((object mortgage) &optional (print nil))
  "run the amortization of the mortgage outout
(list count years total remaining-principle interest-payment principle-payment) 
or print the final payment"
  (with-accessors ((principle principle)
		   (payment payment)
		   (interest interest-rate)
		   (frequency payment-frequency)) object
    (let ((period-interest (ecase frequency
			     (:monthly (/ interest 12))
			     (:bi-weekly-24 (/ interest 24))
			     (:bi-weekly (/ interest 26))))
	  (divisor (ecase frequency
		     (:monthly 12)
		     (:bi-weekly-24 24)
		     (:bi-weekly 26))) ;; accelerated
	  (accum nil))
      (do* ((count 0 (1+ count))
	    (total 0 (* payment count))
	    (years 0 (float (/ count divisor)))
	    (interest-payment (* principle period-interest) (* remaining-principle period-interest))
	    (principle-payment (- payment interest-payment) (- payment interest-payment))
	    (remaining-principle principle (- remaining-principle principle-payment)))
	   ((< remaining-principle principle-payment)
	    (if print
		(format t "payments ~d, years ~d, total ~d, principle ~d, interest-payment ~d, principle-payment ~d~%"
			count years total remaining-principle interest-payment principle-payment)
		(nreverse accum)))
	(push (list count years total remaining-principle interest-payment principle-payment) accum)))))

(defmethod amortization-period ((object mortgage) period)
  "run the amortization of the mortgage for a period 
 count years remaining-principle total-paid principle-paid interest-paid"
  (with-accessors ((principle principle)
		   (payment payment)
		   (interest interest-rate)
		   (frequency payment-frequency)) object
    (let ((period-interest (ecase frequency
			     (:monthly (/ interest 12))
			     (:bi-weekly-24 (/ interest 24))
			     (:bi-weekly (/ interest 26))))
	  (divisor (ecase frequency
		     (:monthly 12)
		     (:bi-weekly-24 24)
		     (:bi-weekly 26))))
      (do* ((count 0 (1+ count))
	    (total 0 (* payment count))
	    (years 0 (float (/ count divisor)))
	    (interest-payment (* principle period-interest) (* remaining-principle period-interest))
	    (interest-paid 0 (+ interest-paid interest-payment))
	    (principle-payment (- payment interest-payment) (- payment interest-payment))
	    (principle-paid 0 (+ principle-paid principle-payment))
	    (remaining-principle principle (- remaining-principle principle-payment)))
	   ((> years period)
	    (format nil "Years ~$~%
Payments ~$~%
Remaining principle ~$~%
Total paid ~$~%
Principle-paid ~$~%
Interest-paid ~$~%" years count remaining-principle total  principle-paid interest-paid))))))

(amortization-period *mortgage-3* 16)

;;; Plots
(defmethod amortization-plot ((object mortgage))
  "Plot an of an amortization"
  (let* (;; (name-string (concatenate 'string (string (type-of object)) "-"
	 ;; 			   (label object)))
	 (file-name (merge-pathnames *figures-dir*
				     (concatenate 'string (label object) ".pdf")))
	 (dataset (amortization object)))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output file-name
			     :terminal '(pdfcairo enhanced font "Verdana,10")
			     :key '(left)
			     ;;:logscale '(y)
			     ;;:title 
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
       :using '("2:3") :with '(lines) :title "paid")
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
       :using '("2:5 w lines title 'interest payment'"))
            (eazy-gnuplot:plot
       (lambda ()
	 (dolist (row dataset)
	   ;; count years total remaining-principle interest-payment principle-payment
	   (format t "~&~{~d ~d ~d ~d ~d ~d~%~}" row)))
       :using '("2:6 w lines title 'principle payment'" ))) file-name))


(defun debt-barchart (&rest lod)
  "Accepts a list of debits -> barchart"
  (let* ((file-name (merge-pathnames *figures-dir* "debit_barchart.pdf")))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output file-name
			     :terminal '(pdfcairo enhanced font "Verdana,10")
			     :key '(right)
                             ;;:title 
                             :xlabel ""
                             :ylabel "value"
;;                             :yrange (list "[0:]")
                             :ytic '(nomirror font ",8")
                             :xtic '(textcolor rgb ("'#000000'") nomirror font ",8"  rotate by -45)
                             :tic '(nomirror out scale 0.75)
                             :border '(11 front linecolor rgb ("'#808080'") lt 1)
                             :boxwidth '(0.9)
			     :style '(histogram clustered)
			     :style '(data histograms)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 lt 1 lc rgb ("'#4682b4'"))
			     :style '(line 2 lt 1 lc rgb ("'#cc0000'")))
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (obj lod)
	   (with-accessors
		 ((label label)
		  (original-balance original-balance)) obj
	     (format t "~s ~a~%" label original-balance))))
       :using '(2 "xtic(1) linestyle 1") :title "original balance")
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (obj lod)
	   (with-accessors
		 ((label label)
		  (principle principle)
		  (rate interest-rate)) obj
	     (format t "~s ~a ~s~%" label principle rate))))
       :using '(2 "xtic(1) linestyle 2") :title "principle" 
       :using '("0:2:2 with labels")) file-name)))

;;(debt-barchart *mortgage-1* *mortgage-2* *mortgage-3* *credit-line-1* *credit-line-2*)




