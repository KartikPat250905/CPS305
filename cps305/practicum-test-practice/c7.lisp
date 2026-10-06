;takes a positive integer. Repeatedly apply: if even, divide by 2, otherwise multiply by 3 and add 1. Return the number of steps needed to reach 1.

(defun COLLATZ-STEPS (n) 
    (do
        (
            (t1 n)
            (steps 0 (+ 1 steps))
        )
        ((= t1 1) steps)
        (
            cond
            ((evenp t1) (setf t1 (/ t1 2)))
            (t (setf t1 (+ 1 (* t1 3))))
        )
    )
)