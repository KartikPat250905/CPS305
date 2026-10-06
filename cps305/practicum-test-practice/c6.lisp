;uses a DO loop to step through two equal-length lists in parallel and returns the sum of products.

(defun DOT-PRODUCT (l1 l2) 
    (DO
        (
            (t1 l1 (cdr t1))
            (t2 l2 (cdr t2))
            (result 0 (+ (* (car t1) (car t2)) result))
        )
        ((null t1) result)
    )
)