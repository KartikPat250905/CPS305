(defun list-sum (lst)
    (
        cond
        ((null lst) 0)
        (t (+ (car lst) (list-sum(cdr lst))))
    )
)
(print   (LIST-SUM (LIST)))