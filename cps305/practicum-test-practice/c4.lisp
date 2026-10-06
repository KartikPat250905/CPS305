; takes a non-negative integer and returns the sum of its digits, using DO and mod / floor.

(defun DIGIT-SUM (n)
    (DO
        (
            (current n (floor current 10))
            (sum 0 (+ sum (mod current 10)))
        )
        ((zerop current) sum)
    )
)