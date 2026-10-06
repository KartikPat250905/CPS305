; is a tail-recursive reverse using LABELS with an accumulator. No reverse or append.

(defun MY-REVERSE (lst)
    (let (
        (result '())
    )
    (LABELS
        (
            (helper (lst result)
                (
                    cond
                    ((null lst) result)
                    (t (helper (cdr lst) (cons (car lst) result)))
                )
            )
        )   
        (helper lst result)
    )
    )
)