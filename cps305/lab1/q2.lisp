(defun CALCULATE-PARKING-FEE (hours is_weekend)
    (let* 
        (
            (
                base_rate (
                cond ((<= hours 2) 4)
                (t 3)
                )
            )
            (
                hourly_total (
                    * base_rate hours
                )
            )
        )
        (cond
            (is_weekend (+ 5 hourly_total))
            (t hourly_total)
        )
    )
)