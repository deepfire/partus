(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (when (or test size rehash-size rehash-threshold)
    (warn "MAKE-HASH-TABLE: options not supported."))
  (funcall #''("dict")))

(defun hash-table-p (x)
  ;; we should hook DEFTYPE/TYPEP into this
  (not (not (funcall #''("isinstance") x (ref '("dict"))))))

(defun hash-table-count (hash-table)
  (funcall #''("len") hash-table))

(defun hash-table-rehash-size (x)
  (not-implemented 'hash-table-rehash-size))

(defun hash-table-rehash-threshold (x)
  (not-implemented 'hash-table-rehash-threshold))

(defun hash-table-size (x)
  (not-implemented 'hash-table-size))

(defun hash-table-test (x)
  (not-implemented 'hash-table-test))

(defun gethash (key hash-table &optional default-value)
  ;; Unregistered Issue COMPLIANCE-GETHASH-VALUES-MULTIPLE
  (if (primitive '("in_") key hash-table)
      (primitive '("index") hash-table key)
      default-value))

(defun (setf gethash) (value key hash-table)
  (puthash value key hash-table))

;; disabled due to:
;; ValueError: expression must have Del context but has Load instead
#+nil
(defun remhash (key hash-table)
  (primitive '("delete") (ir-args (primitive '("index") hash-table key)
                                  ("deletep" . t))))

(defun maphash (fn hash-table)
  (let ((x (gensym "X")))
    (primitive '("filtermap") x (funcall (primitive '("attr") hash-table "items"))
               (funcall fn (primitive '("index") x 0) (primitive '("index") x 1))
               (ref '("None")))
    nil))

(defmacro with-hash-table-iterator (&body body)
  (not-implemented 'with-hash-table-iterator))

(defun clrhash (hash-table)
  (funcall (primitive '("attr") hash-table "clear")))

(defun sxhash (key)
  (funcall #''("hash") key))

#+nil
(defun puthash (value key hash-table)
  (primitive '("raw") '"Assign"
             (primitive '("vector")
                        (primitive '("raw") '"Subscript"
                                   hash-table
                                   (primitive '("raw") '"Index" key)
                                   (primitive '("raw") '"Load")))
             value))
