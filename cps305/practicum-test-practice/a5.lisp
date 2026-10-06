;returns ZERO if n is 0. Otherwise it returns POSITIVE-EVEN, POSITIVE-ODD, NEGATIVE-EVEN or NEGATIVE-ODD.
;Use any conditional you like, but the logic should be clean.

(defun CLASSIFY-NUMBER (n)
    (cond
        ((= n 0) 'ZERO)
        ((and (> n 0) (evenp n)) 'POSITIVE-EVEN)
        ((and (> n 0) (oddp n)) 'POSITIVE-ODD)
        ((and (< n 0) (evenp n)) 'NEGATIVE-EVEN)
        ((and (< n 0) (oddp n)) 'NEGATIVE-ODD)
    )
)