(defun BUILD-DYNAMIC-ARRAY (lst)
    (
        let
        (
            (result (make-ARRAY (length lst) :adjustable t :fill-pointer 0))
        )
        (
            dolist (x lst)
                (vector-push-extend x result)
        )
        result
    )
)