(defun generate-squares (n)
    (let ((squares-list '()))
        (dotimes (i n (reverse squares-list))
            (push (* (1+ i) (1+ i)) squares-list)
        )
    )
)

(print (GENERATE-SQUARES 3))