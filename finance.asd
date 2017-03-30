;;;; finance.asd

(asdf:defsystem #:finance
  :description "Describe finance here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-csv
               #:clsql
               #:clsql-sqlite3
               #:lquery
               #:alexandria
               #:eazy-gnuplot
               #:parse-number
               #:local-time
               #:net-telent-date
               #:cl-ppcre
               #:split-sequence)
  :components ((:file "package")
               (:file "finance")
               (:file "utilities")
               (:file "parse")))
