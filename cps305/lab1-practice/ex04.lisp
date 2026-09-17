(defun COMPUTE-PAYMENT (hours rate tier)
  (cond
    ((string= tier "STANDARD") (* hours rate))
    ((string= tier "OVERTIME") (* hours rate 1.5))
    ((string= tier "BONUS") (+ 50 (* hours rate)))
    (t 0)
  )
  
)



(print (COMPUTE-PAYMENT 10 20 'UNKNOWN))