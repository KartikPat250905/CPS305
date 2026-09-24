(defun count-atoms (tree)
  (cond
    ((null tree) 0)
    ((atom tree) 1)
    (t (+ (count-atoms (car tree)) (count-atoms (cdr tree))))))


(print (COUNT-ATOMS (LIST 1 (LIST 2 3) 4)))