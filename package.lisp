;;;; package.lisp

(defpackage #:finance
  (:use #:cl))

(rename-package "CL-PPCRE" "CL-PPCRE" '("RE"))
(in-package :finance)
(defparameter *statements* #P"/home/mpah/Documents/Personal/bank_statements/")
