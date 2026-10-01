(defun find-first (numbers y f)
    (dolist (x numbers)
        (cond 
            ((funcall f x y) (return x))
        )
    )
)

(print   (FIND-FIRST (LIST 10 7 21 4 35) 7 #'=))