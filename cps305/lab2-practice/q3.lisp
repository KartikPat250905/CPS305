(defun keep-evens (lst)
    (labels (
        (
            helper (lst newlst)
            (
                cond
                ((null lst) newlst)
                ((evenp (car lst)) (helper (cdr lst) (append newlst (list (car lst)))))
                (t (helper (cdr lst) newlst))
            )
        )
    )
    (helper lst '()))
)


(print (KEEP-EVENS (LIST 2 4 6)))