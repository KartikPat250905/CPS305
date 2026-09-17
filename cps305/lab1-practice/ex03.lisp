(defun IN-BOUNDS-P (VAL LOW HIGH)
  (cond 
    ((and (< LOW VAL) (< VAL HIGH)) t)
    (t NIL)
  )  
)


(print (IN-BOUNDS-P -2 -5 0))