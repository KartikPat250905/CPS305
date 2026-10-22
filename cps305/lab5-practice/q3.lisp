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

(defun my-car (lst)
    (if (null (aref lst 0))
        nil
        (aref (aref *cell-array* (aref lst 0)) 0)
    )
)

(defun make-empty-list ()
  (vector nil 0))