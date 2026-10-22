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

(defun my-cons (item lst)
    (let ((
        new-index (allocate-cell item (aref lst 0))
    ))
    (setf (aref lst 0) new-index)
    (setf (aref lst 1) (1+ (aref lst 1)))
    lst))

(defun make-empty-list ()
  (vector nil 0))

