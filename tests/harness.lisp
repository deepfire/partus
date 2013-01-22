(eval-when (:compile-toplevel)
  (impl-call "summary_debug")
  (setq *reader-trace-qqexpansion* t))

(defmacro define-test (name form &body invariants)
  (let ((testname (impl-call "do_intern" (format nil "TEST-%s" name)))
        (ivb (mapcar (lambda (x) (list (gensym) x)) invariants))
        (ivf (mapcar (lambda (x) (list (gensym) `(quote ,x))) invariants)))
    `(progn
       ,form
       (let (,@ivf ,@ivb)
         (if (and ,(mapcar #'car ivb))
             (progn (format t ";%55s: ok" ,name) (terpri))
             (error "Test %s failed invariant %s."
                    ,testname (cond ,@(mapcar (lambda (ivb ivf)
                                                `((not ,(car ivb)) ,(car ivf)))
                                              ivb ivf))))))))
