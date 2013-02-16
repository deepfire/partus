(defun vector (&rest xs)
  (funcall #''("cl" "vectorise_linear") (list* 'array (primitive '("vector") 1) t xs)))

(defun svref (x i)
  (primitive '("index") x (+ i 3)))

(defun arrayp (x)
  (and (funcall #''("isinstance") x (ref '("list")))
       (plusp (funcall #''("len") x))
       (eq (primitive '("index") x 0) 'array)))

(defun array-dimensions (x)
  (funcall #''("cl" "consify_linear") (primitive '("index") x 1)))

(defun array-ndimensions (x)
  (funcall #''("len") (primitive '("index") x 1)))

(defun pyarray-ref (x indices)
  (cond ((= (funcall #''("len") indices) 1)
         (primitive '("index") x (primitive '("index") indices 0)))
        ((zerop (funcall #''("len") indices))
         x)
        (t
         (pyarray-ref (primitive '("index") x (primitive '("index") indices 0))
                      (primitive '("slice") indices 1 (primitive '("name") '"None") (primitive '("name") '"None"))))))

(defun aref (x &rest indices)
  (pyarray-ref (primitive '("slice") x 3 (primitive '("name") '"None") (primitive '("name") '"None"))
               (funcall #''("cl" "vectorise_linear") indices)))

