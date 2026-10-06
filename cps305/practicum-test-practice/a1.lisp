(defun TRIANGLE-VALID-P(a b c)
        (if 
            (and (> (+ a b) c) (> (+ b c) a) (> (+ a c) b))
            t 
            nil
        )
)