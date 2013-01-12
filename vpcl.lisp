(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))

(defmacro defun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel)
       ;; -compile-and-load-function() expects the compiler-level part of function to be present.
       (funcall #''("cl" "compiler_defun") ',name nil))
     (eval-when (:load-toplevel :execute)
       (ir-args
        (lambda ,lambda-list
          (block ,name
            ,@body))
        ("name" . ,name)
        ("globalp" . t)
        ("decorators" (apply #''("cl" "set_function_definition")
                             (apply #''("globals") 'nil)
                             ',name '(lambda ,lambda-list ,@body)
                             'nil)))
       'nil)))

(defun all-hail-vpcl ()
  (format t "; Greetings, this is VPCL here..  You seem bothered?")
  (terpri))

(all-hail-vpcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun not (x)
    (primitive '("not_") x)))

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

(defmacro unless (test &body body)
  `(if (not ,test)
       (progn ,@body)
       nil))

(defmacro setf (target value)
  (if (symbolp target)
      `(setq target value)
      (error "SETF: not implemented: non-symbol target.")))

(defmacro push (x xs)
  `(setf ,xs (cons ,x ,xs)))

(defmacro pop (xs)
  (let ((oldxs (gensym)))
    `(let ((,oldxs ,xs))
       (setf ,xs (cdr ,oldxs))
       (car ,oldxs))))

(defmacro or (&rest forms)
  (if (not (cdr forms)) ;; Handles the case of empty FORMS as well.
      (first forms)
      (let ((tn (gensym "OR-")))
        `(let ((,tn ,(first forms)))
           (if ,tn ,tn (or ,@(cdr forms)))))))

(defmacro and (&rest forms)
  (if (not forms)
      t
      (if (not (cdr forms))
          (first forms)
          `(if (not ,(first forms))
               nil
               (and ,@(cdr forms))))))

(defmacro cond (&rest clauses)
  (when clauses
    (let ((clause (first clauses))
          (rest (rest clauses)))
      (when (or (not clause)
                (cddr clause))
        (error "Invalid COND form: %s" `(cond ,@clauses)))
      (if (cdr clause)
          `(if ,(first clause)
               ,(second clause)
               (cond ,@rest))
          (let ((tn (gensym)))
            `(let ((,tn ,(first clause)))
               (if ,tn
                   ,tn
                   (cond ,@rest))))))))

(defmacro defconstant (name &optional value documentation)
  `(progn
     (eval-when (:compile-toplevel)
       (funcall #''("cl" "compiler_defconstant") ',name ,value))
     (funcall #''("cl" "compiler_defconstant") ',name ,value)
     nil))

(defmacro defvar (name &optional value documentation)
  `(progn
     (eval-when (:compile-toplevel)
       (funcall #''("cl" "compiler_defvar") ',name ,value))
     (funcall #''("cl" "compiler_defvar") ',name ,value)
     nil))

(defmacro defparameter (name &optional value documentation)
  `(progn
     (eval-when (:compile-toplevel)
       (funcall #''("cl" "compiler_defparameter") ',name ,value))
     (funcall #''("cl" "compiler_defparameter") ',name ,value)
     nil))

;;;
;;; Mockery
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-entity-ignorator (kind name)
    `(progn 
       (eval-when (:compile-toplevel)
         (format t "; ignoring definition for %s %s" ,kind ',name)
         (terpri))
       (format t "; ignoring definition for %s %s" ,kind ',name)
       (terpri))))

(defmacro defpackage (name &rest rest)
  (emit-entity-ignorator "package" name))

(defmacro deftype (name supers slots &body class-options)
  (emit-entity-ignorator "type" name))

(defmacro define-condition (name super slots &rest rest)
  (emit-entity-ignorator "condition" name))

(defmacro defclass (name supers slots &body class-options)
  (emit-entity-ignorator "class" name))

(defmacro declaim (&body declamations)
  (emit-entity-ignorator "declamations" declamations))
