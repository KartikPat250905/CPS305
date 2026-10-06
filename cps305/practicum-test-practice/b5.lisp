;takes two ascending-sorted lists of numbers, possibly of different lengths, and returns one sorted list. Don't call sort.

(defun MERGE-SORTED (l1 l2)
    (labels 
        (
            (
                helper (l1 l2 result)
                (
                    cond 
                    ((and (null l1) (null l2)) (reverse result))
                    ((and (null l1) (not (null l2))) (helper l1 (cdr l2) (cons (car l2) result)))
                    ((and (null l2) (not (null l1))) (helper (cdr l1) l2 (cons (car l1) result)))
                    ((> (car l1) (car l2)) (helper l1 (cdr l2) (cons (car l2) result)))
                    (t (helper (cdr l1) l2 (cons (car l1) result)))
                )
            )
        )
        (helper l1 l2 '())
    )
)