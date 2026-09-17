(defun EVALUATE-SCORE (score)
  (cond
    ((>= score 90) "EXCELLENT")
    ((and (>= score 50) (<= score 89)) "PASS")
    (t "FAIL")
  )
)

(print(EVALUATE-SCORE 49))