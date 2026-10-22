(defun DISTANCE-BETWEEN-POINTS (l1 l2)
    (let (
        (x1 (first l1))
        (x2 (first l2))
        (y1 (second l1))
        (y2 (second l2))
    )
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
)