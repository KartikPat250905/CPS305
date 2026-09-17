(defun ABSOLUTE-DIFFERENCE (x y)
  (let ((result (- x y)))
    (if (> result 0)
        result
        (- result))
   )
)
(print(ABSOLUTE-DIFFERENCE 10 4))