(defun CALCULATE-DELIVERY-FEE (dist is_mem)
  (let ((total (cond ((< dist 5) 5) (t 10))))
    (if is_mem
        (- total 2)
        total)
    )
)


(print (CALCULATE-DELIVERY-FEE  10 NIL))