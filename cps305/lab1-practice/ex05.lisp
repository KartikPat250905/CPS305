(defun PROCESS-TRANSACTION (balance amount type)
  (cond
    ((string= type "DEPOSIT") (+ balance amount))
    ((string= type "WITHDRAW") (if (>= balance amount) (- balance amount) balance))
    (t balance)
  )  
)


(print (PROCESS-TRANSACTION 100 50 'TRANSFER))