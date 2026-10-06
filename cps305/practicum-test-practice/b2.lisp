

(defun count-greater(lst n)
    (
        cond
        ((null lst) 0)
        ((> (car lst) n) (+ 1 (count-greater (cdr lst) n)))
        (t (count-greater (cdr lst) n))
    )
)
