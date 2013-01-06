(defmacro defun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel)
       ;; -compile-and-load-function() expects the compiler-level part of function to be present.
       (funcall (function (quote ("cl" "compiler_defun"))) ',name 'nil))
     (eval-when (:load-toplevel :execute)
       (ir-args
        (lambda ,lambda-list
          (block ,name
            ,@body))
        ("name" . ,name)
        ("globalp" . t)
        ("decorators" (apply (function (quote ("cl" "set_function_definition")))
                             (apply (function (quote ("globals"))) 'nil)
                             ',name nil
                             'nil)))
       ;; ',name '(lambda ,lambda-list ,@body))))
                             
       'nil)))


;; (eval-when (:compile-toplevel)
;;   (funcall (function (quote ("cl" "dbgsetup")))))
;; (eval-when (:compile-toplevel)
;;   (setq (quote ("cl" "__enable_matcher_tracing__")) t))

(defun %test-defun (&optional (basis 0) &key (x 1) y (z 3) &allow-other-keys &aux
                    (fmtargs (list basis x y z)))
  (apply (function format) t "Hello from DEFUN TEST basis:%s  x:%s  y:%s  z:%s" fmtargs)
  (terpri))

(%test-defun 42 :y 2 :x 3.14159 :ignore "ignored")
(%test-defun)

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

(defmacro setf (target value)
  (if (symbolp target)
      `(setq target value)
      (error "SETF: not implemented: non-symbol target.")))

(defmacro push (x xs)
  (if (symbolp xs)
      `(setq ,xs (cons ,x ,xs))
      (error "PUSH: not implemented: non-symbol target.")))

(defmacro pop (xs)
  (let ((oldxs (gensym)))
    (if (symbolp xs)
        `(let ((,oldxs ,xs))
           (setq ,xs (cdr ,oldxs))
           (car ,oldxs))
        (error "POP: not implemented: non-symbol target."))))
