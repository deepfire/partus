;;  Dictionary:
;;
;;  Intermarker :: An intermediate representation node, serving to mediate quasiquotation
;;                 matters between the reader and the quasiquotation expander.  One of
;;                 (QUASIQUOTE <x>), (COMMA <x>) or (SPLICE <x>).

(defun simplify-append (x)
  (flet ((append-insulated-p (x)
           (and (consp x) (eq (first x) 'list)
                (consp (rest x))
                (null (cddr x))))
         (try-simplify-list* (xs tail)
           (if (endp (rest xs))
               `(cons ,(second (first xs)) ,tail)
               `(list* ,@(mapcar #'second xs) ,tail))))
    (let* ((pieces (rest x))
           (butlast (butlast pieces))
           (last (last x))
           (lastcar (car last)))
      (cond ((endp (cddr x))
             (second x))
            ((not (every #'append-insulated-p butlast))
             (let ((ninsulated (position-if-not #'append-insulated-p butlast)))
               (if (zerop ninsulated)
                   x
                   (try-simplify-list* (subseq butlast 0 ninsulated) `(append ,@(subseq butlast ninsulated) ,lastcar)))))
            ((append-insulated-p lastcar)
             `(list ,@(mapcar #'second pieces)))
            (t
             (try-simplify-list* butlast lastcar))))))

(defun self-evaluating-p (x)
  (typep x '(or number string character (member t nil pi))))

#+nil
(defun apply-layer-of-quasiquoting (x)
  (labels ((recurse-preserving-intermarkers (x depth)
             (cond ((or (commap x) (splicep))
                    (if (zerop depth)
                        (error "Impossible: comma outside of a backquote.")
                        `(,(first x) ,(recurse-preserving-intermarkers (second x) (1- depth)))))
                   (t
                    ))))
    (quote-outside-intermarkers x))))

(defun quote-around-intermarkers (x)
  (labels ((rec (x)
             (let ((ret (cond ((self-evaluating-p x) x)
                              ((commap x)            (second x))
                              ((splicep x)           (second x))
                              ((atom x)              `(quote ,x))
                              (t
                               (simplify-append `(append ,@(mapcari (lambda (x improper)
                                                                      (funcall (cond ((not improperp)
                                                                                      #'identity)
                                                                                     ((splicep x)
                                                                                      (error "Invalid form: %s." x))
                                                                                     (t
                                                                                      #'second))
                                                                               (rec x)))
                                                                    x
                                                                    :fixup t)))))))
               (if (splicep x)
                   `(list ,ret)
                   ret))))
    (second (rec x))))

(defun expand-quasiquotes (x &optional (depth 0))
  "A complicated, but literal interpretation of CLHS 2.4.6."
  (cond ((quasiquotep x) ;; remove the quasiquote, and recurse -- Q-A-I should never see quasiquotes, as it has no further use
         (quote-around-intermarkers (expand-quasiquotes (second x) (1+ depth))))
        ((or (commap x) (splicep x))
         (if (plusp depth)
             `(,(first x) ,(expand-quasiquotes (second x) (1- depth))) ;; preserve the intermarker
             (error "Comma outside of a backquote.")))
        ((atom x)
         x)
        (t
         (mapcari (lambda (x improperp)
                    (declare (ignore improperp))
                    (expand-quasiquotes x depth))
                  x))))
