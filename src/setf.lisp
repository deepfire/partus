;; Unregistered Issue RUNTIME-CARRY-OVER-OF-SETF-EXPANDERS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *setf-expanders* (make-hash-table))

  (defun atom-setf-expander (form environment)
    (let ((val (gensym)))
      (values nil nil `(,val) `(setq ,form ,val) form)))

  (defun default-compound-form-setf-expander (form environment)
    (declare (ignore environment))
    (let ((temp-vars   (mapcar (lambda (x) (declare (ignore x)) (gensym)) (rest form)))
          (value-forms (rest form))
          (store-var   (gensym "STORE-")))
      (values temp-vars value-forms (list store-var)
              `(funcall #'(setf ,(first form)) ,store-var ,@temp-vars)
              `(,(first form) ,@temp-vars))))

  ;; --> (temp-vars value-forms store-vars storing-form accessing-form)
  (defun get-setf-expansion (form &optional environment)
    (unless (or (symbolp form) (consp form))
      (error "The first argument to GET-SETF-EXPANSION must be either a cons or a symbol, was: %s." form))
    (if (consp form)
        (let ((name (first form)))
          (unless (symbolp name)
            (error "Compound generalised reference must be named by a symbol."))
          (funcall (gethash name *setf-expanders* #'default-compound-form-setf-expander)
                   form environment))
        (atom-setf-expander form environment)))

  (defun expand-one-setf-pair (place value)
    (multiple-value-bind (temp-vars value-forms store-vars store-form) (get-setf-expansion place)
      `(let* (,@(mapcar #'list temp-vars value-forms))
         ,(if (> (length store-vars) 1)
              `(multiple-value-bind (,@store-vars) ,value
                 ,store-form)
              `(let (,@(when store-vars
                             `((,(first store-vars) ,value))))
                 ,store-form))))))

(defmacro define-setf-expansion (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (puthash (lambda ,lambda-list ,@body) name *setf-expanders*)))

(defmacro setf (&rest forms)
  (unless (evenp (length forms))
    (error "Odd number of arguments to SETF."))
  (when forms
    `(progn
       ,(expand-one-setf-pair (first forms) (second forms))
       (setf ,@(cdr (cdr forms))))))
