
(defun process-list (n)
  (do ((i 1 (* i 2)) ; 1 2 4 8 16
       (j 0)) ; 0 1 2 3 4 
      ((>= j n)) 
    (incf j))) 

(defun MY-COMPLEXITY-ANSWER ()
  'O-N)