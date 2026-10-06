;removes consecutive duplicates, using dolist. Non-adjacent repeats stay.

(defun COMPRESS (lst) 
    (let 
        (
            (result '())
            (prev nil)
        )
        (
            dolist (x lst)
            (
                cond
                ((not (equal x prev)) 
                    (push x result)
                    (setf prev x)
                )
            )
        )
        (reverse result)
    )
)