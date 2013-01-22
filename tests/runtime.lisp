(debug)

(define-test regular-function-definition
    (defun foo (x)
      x)
  (functionp (symbol-function 'foo))
  (functionp (fdefinition 'foo)))