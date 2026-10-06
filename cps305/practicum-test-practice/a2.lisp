;SHIPPING-COST (weight express). Base cost: weight ≤ 1 → 5, weight ≤ 5 → 12, otherwise 20. If express is T, add 10. Use cond.
(defun SHIPPING-COST (weight express)
    (let
        (
            (base-cost (
                cond 
                ((<= weight 1) 5)
                ((<= weight 5) 12)
                (t 20)
            ))
        )
        (if express (+ base-cost 10) base-cost)
    )
)