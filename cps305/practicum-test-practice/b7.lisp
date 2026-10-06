;counts the even integers at any nesting depth. The tree may contain non-numeric atoms such as symbols, which must be skipped without error.

(defun DEEP-COUNT-IF-EVEN (tree) 
    (cond 
        ((null tree) 0)
        ((atom tree) (if (and (numberp tree) (evenp tree)) 1 0))
        (t (+ (deep-count-if-even (car tree)) (deep-count-if-even (cdr tree))))
    )
)