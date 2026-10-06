;uses dolist to find the largest number without calling max. It returns NIL for an empty list.

(defun LIST-MAX (lst) 
    (if (null lst) nil)

    (let (
        (max (nth 0 lst))
    )
    (dolist (x lst)
        (
            cond 
            ((> x max) (setf max x))
        )
    )
    max
    )
)