(defmacro defun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel)
       ;; -compile-and-load-function() expects the compiler-level part of function to be present.
       (apply (function (quote ("cl" "_compiler_defun"))) ',name 'nil
              'nil))
     (eval-when (:load-toplevel :execute)
       (apply (apply (function (quote ("cl" "_set_function_definition")))
                     (apply (function (quote ("globals"))) 'nil) ',name '(lambda ,lambda-list ,@body)
                     'nil)
              (progn
                (def ,name ,lambda-list
                  (block ,name
                    ,@body))
                (function ,name))
              'nil))))

(defun %test-defun ()
  (format t "Hello from DEFUN TEST")
  (terpri))

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
