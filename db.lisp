;;; file db.lisp
;;; db tables and load statements


(defun create-empty-db (&optional (db-file *db-file*))
  "Start over again with an empty db"
  (uiop:delete-file-if-exists db-file)
  (let ((stream (open db-file :direction :output)))
    (close stream))
  (dbi:with-connection (conn :sqlite3 :database-name db-file)
    (let ((query (dbi:prepare conn "create table transactions 
(account text,
number text,
date date,
cheque text,
description text,
description2 text,
cad numeric,
usd numeric,
catagory text,
PRIMARY KEY (account, date, description, cad) )")))
  (dbi:execute query))))



(defun load-csv-file (csv-file &optional (header *header*) (db-file *db-file*))
  "load parsed data from csv file in db"
  (let ((transaction-list (lantern::load-csv csv-file :col-names header)))
    (dbi:with-connection (conn :sqlite3 :database-name db-file)
      (let ((query (dbi:prepare conn "insert or ignore into transactions values(?, ?, ?, ?, ?, ?, ?, ?, ?)")))
	(dolist (row transaction-list)
	  (let ((parsed-row (parse-row row)))
	    (destructuring-bind (&key account number date cheque description description2 cad usd catagory) parsed-row
	      (dbi:execute query account number date cheque description description2 cad usd catagory))))))))




; INSERT INTO players (user_name, age)
  ;; VALUES('steven', 32) 
  ;; ON CONFLICT(user_name) 
  ;; DO UPDATE SET age=excluded.age;
