(defun pairwise-max (l1 l2)
    (do (
        (n1 l1 (cdr n1))
        (n2 l2 (cdr n2))
        (result '())
    )
    ((null n1) (reverse result))
    (cond 
        ((> (car n1) (car n2)) (push (car n1) result))
        (t (push (car n2) result))
    )
    )
)

(print  (PAIRWISE-MAX (LIST 1 5 3) (LIST 4 2 6)))