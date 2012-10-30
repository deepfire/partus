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
                             ',name nil ;; '(lambda ,lambda-list ,@body)
                             'nil)))
       'nil)))

(eval-when (:compile-toplevel)
  (apply (function (quote ("cl" "_dbgsetup"))) 'nil))

(defun %test-defun (&optional (basis 0) &key (x 1) y (z 3) &aux
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
