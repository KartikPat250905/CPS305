(defun IN-RANGE-P (VAL LOW HIGH)
  (IF (and (>= VAL LOW) (>= HIGH VAL))
      t
      nil))
(print(IN-RANGE-P 15 1 10))