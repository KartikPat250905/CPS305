; FACTORIAL-ITERATIVE (n) uses DO.

(defun FACTORIAL-ITERATIVE (n)
    (DO*
        (
            (i 1 (+ 1 i))
            (result 1 (* result i))
        )
        ((= i n) result)
    )
)