(defmacro defun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel)
       ;; -compile-and-load-function() expects the compiler-level part of function to be present.
       (apply (function (quote ("cl" "_compiler_defun"))) ',name 'nil
              'nil))
     (eval-when (:load-toplevel :execute)
       (ir-args
        (lambda ,lambda-list
          (block ,name
            ,@body))
        ("name" . ,name)
        ("decorators" (apply (function (quote ("cl" "_set_function_definition")))
                             (apply (function (quote ("globals"))) 'nil)
                             ',name '(lambda ,lambda-list ,@body)
                             'nil)))
       'nil)))

(apply (function (quote ("cl" "_setup_tracing"))) 'nil)

(defun %test-defun (&optional (basis 0) &key (x 1) y (z 3))
  (format t "Hello from DEFUN TEST basis:%s  x:%s  y:%s  z:%s" basis x y z)
  (terpri))

(%test-defun 42 :y 2 :x 3.14159)
(%test-defun)

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

(defmacro setf (target value)
  `(setq target value))

(defmacro push (x xs)
  (error "PUSH: not implemented."))

(defmacro pop (xs)
  (error "POP: not implemented."))
