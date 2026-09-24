(defun count-odd-numbers (lst)
    (labels (
        (tail-helper (lst acc)
            (
                cond
                ((null lst) acc)
                ((oddp (car lst)) (tail-helper (cdr lst) (+ acc 1)))
                (t (tail-helper (cdr lst) acc))
            )
        )
    )
    
    (tail-helper lst 0))
)

(print  (COUNT-ODD-NUMBERS (LIST 7)))