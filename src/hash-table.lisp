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

(defun gethash (key hash-table)
  ;; Unregistered Issue COMPLIANCE-GETHASH-VALUES-MULTIPLE
  (if (primitive '("in_") key hash-table)
      (primitive '("index") hash-table key)
      nil))

(defun (setf gethash) (value key hash-table)
  (puthash value key hash-table))

(defun remhash (key hash-table)
  (primitive '("delete") (primitive '("index") hash-table key)))

(defun maphash (fn hash-table)
  (not-implemented 'maphash))

(defmacro with-hash-table-iterator (&body body)
  (not-implemented 'with-hash-table-iterator))

(defun clrhash (hash-table)
  (funcall (primitive '("attr") hash-table "clear")))

(defun sxhash (key)
  (funcall #''("hash") key))

;;;
(defun puthash (value key hash-table)
  (primitive '("assign") (primitive '("index") hash-table key) value))