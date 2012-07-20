(in-package :common-lisp)

(defmacro defpackage (name &body options)
  (format t "Yay, expanding DEFPACKAGE %s!" name)
  `(format t "Yay, executing expanded DEFPACKAGE %s!" ,name))

(defpackage :vpcl)
