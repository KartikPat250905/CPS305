(defun COMPUTE-SHIPPING-COST (weight distance mode)
    (
        cond
        ((eq mode 'STANDARD) (+ (* weight 1.5) (* 0.1 distance)))
        ((eq mode 'EXPRESS) (+ (* weight 3.0) (* 0.25 distance)))
        ((eq mode 'FREIGHT) (+ (* weight 0.5) (* 0.05 distance)))
        (t -1)
    )
)