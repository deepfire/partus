(defvar *setf-functions* (make-hash-table))

(defun register-setf-function (name fn)
  (puthash fn name *setf-functions*))

