; returns a list of the first n odd numbers, using dotimes.

(defun ODD-NUMBERS (n) 
    (let (
        (result '())
        (current 1)
    )
    (dotimes (count n (reverse result)) 
        (push current result)
        (setf current (+ 2 current))
    )
    )
)