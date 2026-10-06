;returns the product of all numbers in the list. The product of an empty list is 1.
(defun LIST-PRODUCT (lst)
    (
        if (null lst) 1 (
            * (car lst) (list-product(cdr lst))
        )
    )
)