;;;; finance.asd

(asdf:defsystem #:finance
  :description "Describe finance here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-csv
               #:cl-dbi
	       #:clsql
	       #:plump
               #:lquery
               #:alexandria
               #:eazy-gnuplot
               #:parse-number
               #:local-time
               #:net-telent-date
               #:cl-ppcre
               #:split-sequence
;	       #:lantern
	       )
  :components ((:file "package")
	       (:file "utilities")
	       (:file "classes")
	       (:file "parameters")
               (:file "finance")
	       (:file "debt")
	       (:file "assets")
	       (:file "taxes")
;;	       (:file "csv")
;;	       (:file "db")
               (:file "pdf") ;; removing this and working with csv files for now
;;             (:file "classify")
	       ))
