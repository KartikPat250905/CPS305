;returns a flat list of all non-NIL atoms in left-to-right order

(defun FLATTEN (tree)
    (cond
        ((null tree) nil)
        ((atom tree) (list tree))
        (t
            (append
                (FLATTEN (car tree))
                (FLATTEN (cdr tree))))))