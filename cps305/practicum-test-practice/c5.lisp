; uses dolist to return a list of two lists, (evens odds), each preserving the original order.
(defun PARTITION-PARITY (lst)
    (let
        (
            (evens '())
            (odds '())
        )
    (dolist (x lst)
        (cond
            ((evenp x) (push x evens))
            (t (push x odds))
        )
    )
    ( list (reverse evens) (reverse odds))
    )
)