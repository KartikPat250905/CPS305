;;; stack.lisp
;;; Stack = #(array top-index max-size)

(defun make-empty-stack (cap)
    (let (
        (array (make-array cap :initial-element nil))
    )
    (vector array -1 cap))
)

(defun stack-empty-p (stk)
    (< (aref stk 1) 0)
)

(defun stack-full-p (stk)
    (= (aref stk 1) (1- (aref stk 2)))
)

(defun stack-push (item stk)
    (if (stack-full-p stk) (error "Stack full"))
    (setf (aref (aref stk 0) (1+ (aref stk 1))) item)
    (setf (aref stk 1) (1+ (aref stk 1)))
    stk
)

(defun stack-pop (stk)
    (if (stack-empty-p stk) (error "Stack Empty"))
    (let (
        (top (aref (aref stk 0) (aref stk 1)))
    )
    (decf (aref stk 1))
    top
    )
)

(defun stack-peek (stk)
    (if (stack-empty-p stk) (error "Stack Empty"))
    (let (
        (top (aref (aref stk 0) (aref stk 1)))
    )
    top
    )
)

(defun stack-size (stk)
    (1+ (aref stk 1))
)

(defun stack-capacity-left (stk)
    (- (aref stk 2) (1+ (aref stk 1)))
)

(defun stack-push-many (lst stk) 
    (dolist (x lst)
        (stack-push x stk)
    )
)

(defun stack-pop-many (n stk)
    (let (
        (result '())
    )
    (dotimes (i n)
        (push (stack-pop stk) result)
    )
    (reverse result)
    )
)