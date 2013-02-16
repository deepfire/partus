(defmacro debug (&optional (enable t))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (impl-call ,(if enable "full_debug" "no_debug"))
     nil))

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))

(defmacro when (test &body body)
  `(if ,test
       (progn ,@body)
       nil))

(defmacro unless (test &body body)
  `(if ,test
       nil
       (progn ,@body)))

(defmacro or (&rest forms)
  (if (cdr forms) ;; Handles the case of empty FORMS as well.
      (let ((tn (gensym "OR-")))
        `(let ((,tn ,(first forms)))
           (if ,tn ,tn (or ,@(cdr forms)))))
      (first forms)))

(defmacro and (&rest forms)
  (if forms
      (if (cdr forms)
          `(if ,(first forms)
               (and ,@(cdr forms))
               nil)
          (first forms))
      t))

(defmacro cond (&rest clauses)
  (when clauses
    (let ((clause (first clauses))
          (rest (rest clauses)))
      (unless (and clause
                   (eq (cddr clause) nil))
        (error "Invalid COND form: %s" (impl-call "pp_consly" `(cond ,@clauses))))
      (if (cdr clause)
          `(if ,(first clause)
               ,(second clause)
               (cond ,@rest))
          (let ((tn (gensym)))
            `(let ((,tn ,(first clause)))
               (if ,tn
                   ,tn
                   (cond ,@rest))))))))

(defmacro defun (name lambda-list &body body)
  (unless (or (symbolp name)
              (and (consp name) (eq (car name) 'setf) (symbolp (car (cdr name)))))
    (error "In DEFUN: invalid function name: %s -- symbolp %s." name (symbolp name)))
  `(progn
     (eval-when (:compile-toplevel)
       ;; -compile-and-load-function() expects the compiler-level part of function to be present.
       (funcall #''("cl" "compiler_defun") ',name nil))
     (eval-when (:load-toplevel :execute)
       (ir-args
        (lambda ,lambda-list
          (block ,(if (symbolp name)
                      name
                      (car (cdr name)))
            ,@body))
        ("name" . ,name)
        ("globalp" . t)
        ("pydecorators" (apply #''("cl" "set_function_definition")
                               (apply #''("globals") 'nil)
                               ',name '(lambda ,lambda-list ,@body)
                               'nil)))
       'nil)))

;; (eval-when (:compile-toplevel)
;;   (funcall #''("cl" "full_debug")))

(defun all-hail-vpcl ()
  (format t "; Greetings, this is VPCL here..  You seem bothered?")
  (terpri))

(all-hail-vpcl)

;;;
;;; The world begins.
;;;
(defun not-implemented (x)
  (error "%s: not implemented." x))

(defmacro defconstant (name &optional value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (funcall #''("cl" "compiler_defconstant") ',name ,value)
     nil))

#+nil
(defmacro defvar (name &optional value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (funcall #''("cl" "compiler_defvar") ',name ,value)
     nil))

(defmacro defparameter (name &optional value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (funcall #''("cl" "compiler_defparameter") ',name ,value)
     nil))

;;
(defun not (x)
  (if (primitive '("in_") x (primitive '("pytuple") nil (ref '("None"))))
      t
      nil))

;;;
;;; Mockery
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-entity-ignorator (kind name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defmacro declare (&rest declarations)
  (emit-entity-ignorator "declaration" declarations))
