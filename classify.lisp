;;; file classify.lispo
;;; Methods and function to classify transactions
(in-package :finance)


(defun list-descriptions (loto)
  "descriptions from list of transaction objects"
(mapcar #'description loto))
