;returns a new list with every occurrence of x removed. Don't use the built-in remove, and compare with eql.

(defun REMOVE-ALL (x lst) 
    (
        cond
        ((null lst) '())
        ((eql x (car lst)) (remove-all x (cdr lst)))
        (t (cons (car lst) (remove-all x (cdr lst))))
    )
)