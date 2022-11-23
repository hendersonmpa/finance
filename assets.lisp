;;;; assets.lisp

(in-package #:finance)


(defun asset-barchart (&rest loa)
  "Accepts a list of assets -> barchart"
  (let* ((file-name (merge-pathnames *figures-dir* "asset_barchart.pdf")))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output file-name
			     :terminal '(pdfcairo enhanced font "Verdana,10")
			     :key '(left)
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
	 (dolist (obj loa)
	   (with-accessors
		 ((label label)
		  (cost book-cost)) obj
	     (format t "~s ~a~%" label cost))))
       :using '(2 "xtic(1) linestyle 1") :title "cost")
      (eazy-gnuplot:plot
       (lambda ()
	 (dolist (obj loa)
	   (with-accessors
		 ((label label)
		  (value value)) obj
	     (format t "~s ~a~%" label value))))
       :using '(2 "xtic(1) linestyle 2") :title "value")) file-name))

;;(asset-barchart *corp* *rsp* *resp* *house*)

