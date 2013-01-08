(defmacro in-package (package)
  `(progn
     (eval-when (:compile-toplevel)
       (funcall #''("cl" "do_in_package") ',package))
     (funcall #''("cl" "do_in_package") ',package)))

;; option::= (:nicknames nickname*)* |
;;           (:documentation string) |
;;           (:use package-name*)* |
;;           (:shadow {symbol-name}*)* |
;;           (:shadowing-import-from package-name {symbol-name}*)* |
;;           (:import-from package-name {symbol-name}*)* |
;;           (:export {symbol-name}*)* |
;;           (:intern {symbol-name}*)* |
;;           (:size integer)
;; The order in which the options appear in a defpackage form is irrelevant. The order in which they are executed is as follows:
;; 1. :shadow and :shadowing-import-from.
;; 2. :use.
;; 3. :import-from and :intern.
;; 4. :export.

;; Shadows are established first, since they might be necessary to block spurious name conflicts
;; when the :use option is processed. The :use option is executed next so that :intern and :export
;; options can refer to normally inherited symbols. The :export option is executed last so that it
;; can refer to symbols created by any of the other options; in particular, shadowing symbols and
;; imported symbols can be made external.

;; If a defpackage form appears as a top level form, all of the actions normally performed by this
;; macro at load time must also be performed at compile time.

;; The macroexpansion of defpackage could usefully canonicalize the names into strings, so that
;; even if a source file has random symbols in the defpackage form, the compiled file would
;; only contain strings.

(defun %do-defpackage (name nicknames documentation use shadow shadowing-import-from import-from export intern size)
  (unless (stringp name)
    (error "%%DO-DEFPACKAGE: NAME must be a string, was: %s." name))
  (when (find-package name)
    (error "Refusing to redefine package %s." name))
  (let ((package (funcall #''("cl" "make_package")
                          name)))
    package))

XXX: requires DOLIST and CASE

(defmacro defpackage (name &rest options)
  (let (nicknames
        documentation
        use
        shadow
        shadowing-import-from
        import-from
        export
        intern
        size)
    (flet ((fail (control &rest args)
             (apply #'error (concatenate 'string (format nil "Error in DEFPACKAGE %s: " name) control ".") args)))
      (dolist (opt options)
        (unless (consp opt)
          (fail "invalid option specifier %s" opt))
        (case (car opt)
          (:nicknames
           (setq nicknames (append nicknames (cdr opt))))
          (:documentation
           (when documentation
             (fail "too many documentation options"))
           (setq documentation (second opt)))
          (:use
           (setq use (append use (cdr opt))))
          (:shadow
           (setq shadow (append shadow (cdr opt))))
          (:shadowing-import-from)
          (:import-from)
          (:export
           (setq export (append export (cdr opt))))
          (:intern
           (setq intern (append intern (cdr opt))))
          (:size
           (when size
             (fail "too many size options"))
           (setq size (second opt)))
          (t
           (fail "unknown option %s" opt)))))
    `(progn
       (eval-when (:compile-toplevel)
         (funcall #''("cl" "compiler_defpackage") ',name))
       (funcall #''("cl" "do_defpackage") ',name))))
