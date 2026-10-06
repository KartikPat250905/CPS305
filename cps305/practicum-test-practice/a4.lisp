;Apply the discount first (price - price*discount/100), then add tax on the discounted amount. Use let*.

(defun FINAL-PRICE (price tax-pct discount-pct)
  (let* ((discounted (- price (* price (/ discount-pct 100.0))))
         (discounted-taxed (+ discounted (* discounted (/ tax-pct 100.0)))))
    discounted-taxed))