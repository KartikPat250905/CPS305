(defun BUILD-DYNAMIC-ARRAY (lst)
    (let (
        (result (make-array 0 :adjustable t :fill-pointer 0))
    )
    (dolist (x lst)
        (vector-push-extend x result))
    result)
)