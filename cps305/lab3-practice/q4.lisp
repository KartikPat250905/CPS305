(defun running-sum (lst)
    (let ((result '()) (sum 0))
        (dolist
            (x lst)
            (setf sum (+ sum x))
            (push sum result)
        )
        (reverse result)
    )
)

(print  (RUNNING-SUM (LIST 1 2 3 4)))