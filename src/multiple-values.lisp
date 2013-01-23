(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))

(defmacro values-list (list)
  `(apply #'values ,list))

(defmacro multiple-value-bind (value-vars form &body body)
  (let ((rest (gensym)))
    `(multiple-value-call
         (lambda (&optional ,@value-vars &rest ,rest)
           (declare (ignore ,rest))
           ,@body)
       ,form)))