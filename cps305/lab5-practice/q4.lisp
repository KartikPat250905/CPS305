(defparameter *array-size* 100)
(defparameter *cell-array* (make-array *array-size* :initial-element nil))
(defparameter *free-index* 0)

(defun allocate-cell (data next-index)
  (if (>= *free-index* *array-size*)
      (error "Backing array is full.")
      (let ((index *free-index*))
        (setf (aref *cell-array* index) (vector data next-index))
        (incf *free-index*)
        index)))

(defun my-cdr (lst)
    (when (null (aref lst 0))
        (return-from my-cdr lst)
    )
    (setf (aref lst 0) (aref (aref *cell-array* (aref lst 0)) 1) )
    (setf (aref lst 1) (1- (aref lst 1)))
    lst
)

(defun make-empty-list ()
  (vector nil 0))