(defun pairwise-sum (l1 l2)
    (labels (
        (
            helper (l1 l2 acclst)
            (
                cond
                ((null l1) acclst)
                (t (helper (cdr l1) (cdr l2) (append acclst (list (+ (car l1) (car l2))))))
            )
        )
    )
    (helper l1 l2 '()))
)


(print         (PAIRWISE-SUM (LIST 1 2) (LIST 3 4)))