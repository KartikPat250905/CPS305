(defun fibonacci-iterative (n)
    (
        do (
            (curr 0 next)
            (next 1 (+ curr next))
            (count 0 (+ count 1))
        )
        ((= count n) curr)
    )
)

(print (FIBONACCI-ITERATIVE 6))