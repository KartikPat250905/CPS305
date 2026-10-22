;;;; ============================================================
;;;; MOCK MIDTERM 2 - AUTOMATED TESTS
;;;; ============================================================
;;;; HOW TO USE
;;;;   1. Put your solutions in a file, e.g. mock2-mine.lisp
;;;;      (for Q6, also paste the Lab 5 setup code: *array-size*,
;;;;       *cell-array*, *free-index*, allocate-cell, make-empty-list,
;;;;       my-cons -- this file supplies them only if missing).
;;;;   2. Run everything:
;;;;        sbcl --noinform --load mock2-mine.lisp --load mock2-tests.lisp --eval '(quit)'
;;;;      or in a REPL / OLIVE:
;;;;        (load "mock2-mine.lisp") (load "mock2-tests.lisp")
;;;;   3. Run ONE function while you work on it:
;;;;        (run-fn 'flatten)      ; one function
;;;;        (run-q 3)              ; one question (1-6)
;;;;        (run-all)              ; everything
;;;;
;;;; Legend:  PASS | FAIL (shows expected vs got) | TODO (function not
;;;; written yet) | ERROR (your code crashed on that input)
;;;; ============================================================

#+sbcl (declaim (sb-ext:muffle-conditions style-warning))

(defvar *tests* nil)          ; list of (question fn-name label thunk expected test)
(defvar *pass* 0)
(defvar *fail* 0)
(defvar *todo* 0)

;;; ---- Lab 5 setup, only if the user's file did not define it ----
(unless (boundp '*cell-array*)
  (defparameter *array-size* 100)
  (defparameter *cell-array* (make-array *array-size* :initial-element nil))
  (defparameter *free-index* 0))
(unless (fboundp 'allocate-cell)
  (defun allocate-cell (data next-index)
    (if (>= *free-index* *array-size*)
        (error "Backing array is full.")
        (let ((index *free-index*))
          (setf (aref *cell-array* index) (vector data next-index))
          (incf *free-index*)
          index))))
(unless (fboundp 'make-empty-list)
  (defun make-empty-list () (vector nil 0)))

;;; Helpers for Q6 (these are test utilities, not part of the answer)
(defun fresh-store ()
  (setf *array-size* 100
        *free-index* 0
        *cell-array* (make-array 100 :initial-element nil)))

(defun build-adt (items)
  "Build a list-ADT vector #(head size) holding ITEMS in order, using
   allocate-cell directly (so it does not depend on YOUR my-cons)."
  (fresh-store)
  (let ((head nil))
    (dolist (x (reverse items))
      (setf head (allocate-cell x head)))
    (vector head (length items))))

(defun adt->list (adt)
  "Read the ADT back into an ordinary Lisp list (walks the cells)."
  (let ((res nil) (idx (aref adt 0)))
    (loop while idx
          do (push (aref (aref *cell-array* idx) 0) res)
             (setf idx (aref (aref *cell-array* idx) 1)))
    (nreverse res)))

;;; ---- test definition and running ----
(defmacro deftest (q fn label expr expected &key (test 'equalp))
  `(push (list ,q ',fn ,label (lambda () ,expr) ,expected ',test) *tests*))

(defun run-one (entry)
  (destructuring-bind (q fn label thunk expected test) entry
    (declare (ignore q))
    (handler-case
        (let ((got (funcall thunk)))
          (if (funcall test got expected)
              (progn (incf *pass*)
                     (format t "  PASS  ~a~%" label))
              (progn (incf *fail*)
                     (format t "  FAIL  ~a~%          expected: ~s~%          got:      ~s~%"
                             label expected got))))
      (undefined-function (c)
        (if (eq (cell-error-name c) fn)
            (progn (incf *todo*)
                   (format t "  TODO  ~a   (~a not defined yet)~%" label fn))
            (progn (incf *fail*)
                   (format t "  ERROR ~a   calls undefined function ~a~%"
                           label (cell-error-name c)))))
      (error (c)
        (incf *fail*)
        (format t "  ERROR ~a~%          ~a~%" label c)))))

(defun run-entries (entries)
  (setf *pass* 0 *fail* 0 *todo* 0)
  (let ((last-fn nil))
    (dolist (e entries)
      (unless (eq (second e) last-fn)
        (format t "~%[Q~a] ~(~a~)~%" (first e) (second e))
        (setf last-fn (second e)))
      (run-one e)))
  (format t "~%==== ~a passed, ~a failed, ~a not written yet (of ~a) ====~%"
          *pass* *fail* *todo* (+ *pass* *fail* *todo*))
  (values))

(defun run-all () (run-entries (reverse *tests*)))
(defun run-q (n) (run-entries (remove-if-not (lambda (e) (= (first e) n)) (reverse *tests*))))
(defun run-fn (name)
  (run-entries (remove-if-not (lambda (e) (eq (second e) name)) (reverse *tests*))))

;;; ============================================================
;;; Q1 quick code
;;; ============================================================
(deftest 1 second-last "(second-last '(1 2 3 4)) => 3" (second-last '(1 2 3 4)) 3)
(deftest 1 second-last "two elements => first" (second-last '(a b)) 'a)
(deftest 1 second-last "one element => NIL" (second-last '(a)) nil)
(deftest 1 second-last "empty => NIL" (second-last '()) nil)

(deftest 1 repeat-each "(repeat-each '(a b) 3)" (repeat-each '(a b) 3) '(a a a b b b))
(deftest 1 repeat-each "k = 1 leaves list unchanged" (repeat-each '(1 2 3) 1) '(1 2 3))
(deftest 1 repeat-each "k = 0 gives NIL" (repeat-each '(1 2 3) 0) nil)
(deftest 1 repeat-each "empty list" (repeat-each '() 5) nil)

;;; ============================================================
;;; Q2 recursion
;;; ============================================================
(deftest 2 flatten "nested example" (flatten '(1 (2 (3 4)) ((5)))) '(1 2 3 4 5))
(deftest 2 flatten "already flat" (flatten '(1 2 3)) '(1 2 3))
(deftest 2 flatten "empty list" (flatten '()) nil)
(deftest 2 flatten "empty sublists vanish" (flatten '(() (1 ()) ())) '(1))
(deftest 2 flatten "deep" (flatten '((((a))))) '(a))

(deftest 2 subsets-sum "12 reachable (3+9)" (subsets-sum '(3 9 8 4) 12) t)
(deftest 2 subsets-sum "6 not reachable" (subsets-sum '(3 9 8 4) 6) nil)
(deftest 2 subsets-sum "target 0 (empty subset)" (subsets-sum '(5 6) 0) t)
(deftest 2 subsets-sum "empty list, target 5" (subsets-sum '() 5) nil)
(deftest 2 subsets-sum "whole list" (subsets-sum '(1 2 3) 6) t)
(deftest 2 subsets-sum "too big" (subsets-sum '(1 2 3) 7) nil)

(deftest 2 my-reverse "(1 2 3 4) => (4 3 2 1)" (my-reverse '(1 2 3 4)) '(4 3 2 1))
(deftest 2 my-reverse "empty" (my-reverse '()) nil)
(deftest 2 my-reverse "single" (my-reverse '(a)) '(a))
(deftest 2 my-reverse "nested items stay intact" (my-reverse '((1 2) 3)) '(3 (1 2)))

;;; ============================================================
;;; Q3 loops
;;; ============================================================
(deftest 3 digits-sum "4821 => 15" (digits-sum 4821) 15)
(deftest 3 digits-sum "0 => 0" (digits-sum 0) 0)
(deftest 3 digits-sum "7 => 7" (digits-sum 7) 7)
(deftest 3 digits-sum "1000 => 1" (digits-sum 1000) 1)

(deftest 3 max-run "(1 1 2 2 2 3 2 2) => 3" (max-run '(1 1 2 2 2 3 2 2)) 3)
(deftest 3 max-run "all same" (max-run '(7 7 7 7)) 4)
(deftest 3 max-run "no repeats" (max-run '(1 2 3)) 1)
(deftest 3 max-run "single" (max-run '(9)) 1)
(deftest 3 max-run "empty => 0" (max-run '()) 0)
(deftest 3 max-run "first element is NIL-safe" (max-run '(nil nil a)) 2)

(deftest 3 transpose-pairs "basic" (transpose-pairs '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(deftest 3 transpose-pairs "empty" (transpose-pairs '() '()) nil)
(deftest 3 transpose-pairs "single" (transpose-pairs '(1) '(x)) '((1 x)))

(deftest 3 pascal-row "row 0" (pascal-row 0) '(1))
(deftest 3 pascal-row "row 1" (pascal-row 1) '(1 1))
(deftest 3 pascal-row "row 2" (pascal-row 2) '(1 2 1))
(deftest 3 pascal-row "row 4" (pascal-row 4) '(1 4 6 4 1))
(deftest 3 pascal-row "row 6" (pascal-row 6) '(1 6 15 20 15 6 1))

;;; ============================================================
;;; Q4 arrays  (each test builds a fresh array so mutation is safe)
;;; ============================================================
(deftest 4 reverse-range! "middle range" (reverse-range! (vector 1 2 3 4 5) 1 3) #(1 4 3 2 5))
(deftest 4 reverse-range! "whole array" (reverse-range! (vector 1 2 3 4) 0 3) #(4 3 2 1))
(deftest 4 reverse-range! "lo = hi changes nothing" (reverse-range! (vector 1 2 3) 1 1) #(1 2 3))
(deftest 4 reverse-range! "must modify IN PLACE"
  (let ((a (vector 1 2 3 4 5))) (reverse-range! a 0 2) a) #(3 2 1 4 5))

(deftest 4 array-has-duplicate-p "has duplicate" (array-has-duplicate-p (vector 1 2 3 2)) t :test eq)
(deftest 4 array-has-duplicate-p "no duplicate" (array-has-duplicate-p (vector 1 2 3 4)) nil :test eq)
(deftest 4 array-has-duplicate-p "single element" (array-has-duplicate-p (vector 5)) nil :test eq)
(deftest 4 array-has-duplicate-p "duplicates at both ends" (array-has-duplicate-p (vector 9 1 2 9)) t :test eq)

(deftest 4 merge-arrays "basic" (merge-arrays (vector 1 4 9) (vector 2 3 10)) #(1 2 3 4 9 10))
(deftest 4 merge-arrays "first empty" (merge-arrays (vector) (vector 1 2)) #(1 2))
(deftest 4 merge-arrays "second empty" (merge-arrays (vector 1 2) (vector)) #(1 2))
(deftest 4 merge-arrays "duplicates" (merge-arrays (vector 1 3 3) (vector 3 4)) #(1 3 3 3 4))
(deftest 4 merge-arrays "all of one before other" (merge-arrays (vector 1 2) (vector 8 9)) #(1 2 8 9))

(deftest 4 binary-search "found in middle" (binary-search (vector 1 3 5 7 9) 5) 2)
(deftest 4 binary-search "first" (binary-search (vector 1 3 5 7 9) 1) 0)
(deftest 4 binary-search "last" (binary-search (vector 1 3 5 7 9) 9) 4)
(deftest 4 binary-search "missing => NIL" (binary-search (vector 1 3 5 7 9) 4) nil)
(deftest 4 binary-search "empty => NIL" (binary-search (vector) 4) nil)
(deftest 4 binary-search "single, found" (binary-search (vector 4) 4) 0)

;;; ============================================================
;;; Q5 sorting
;;; ============================================================
(deftest 5 insertion-sort "unsorted" (insertion-sort (vector 7 3 9 1 5)) #(1 3 5 7 9))
(deftest 5 insertion-sort "already sorted" (insertion-sort (vector 1 2 3)) #(1 2 3))
(deftest 5 insertion-sort "reversed" (insertion-sort (vector 5 4 3 2 1)) #(1 2 3 4 5))
(deftest 5 insertion-sort "duplicates" (insertion-sort (vector 3 1 3 1 2)) #(1 1 2 3 3))
(deftest 5 insertion-sort "single" (insertion-sort (vector 4)) #(4))
(deftest 5 insertion-sort "sorts IN PLACE"
  (let ((a (vector 3 2 1))) (insertion-sort a) a) #(1 2 3))

(deftest 5 selection-sort-swaps "returns swap count (needs 2 swaps)"
  (selection-sort-swaps (vector 3 1 2)) 2)
(deftest 5 selection-sort-swaps "already sorted => 0 swaps"
  (selection-sort-swaps (vector 1 2 3 4)) 0)
(deftest 5 selection-sort-swaps "array ends sorted"
  (let ((a (vector 29 10 14 37 13))) (selection-sort-swaps a) a) #(10 13 14 29 37))
(deftest 5 selection-sort-swaps "swap count on 29 10 14 37 13 is 3"
  (selection-sort-swaps (vector 29 10 14 37 13)) 3)

(deftest 5 quicksort3 "basic" (quicksort3 '(4 2 7 1 9 3)) '(1 2 3 4 7 9))
(deftest 5 quicksort3 "duplicates are kept" (quicksort3 '(3 1 3 2 3)) '(1 2 3 3 3))
(deftest 5 quicksort3 "empty" (quicksort3 '()) nil)
(deftest 5 quicksort3 "sorted input" (quicksort3 '(1 2 3 4)) '(1 2 3 4))
(deftest 5 quicksort3 "all equal" (quicksort3 '(5 5 5)) '(5 5 5))
(deftest 5 quicksort3 "does not need to modify its argument"
  (let ((l (list 3 1 2))) (quicksort3 l) l) '(3 1 2))

(deftest 5 isort "basic" (isort '(5 2 4 6 1 3)) '(1 2 3 4 5 6))
(deftest 5 isort "empty" (isort '()) nil)
(deftest 5 isort "duplicates" (isort '(2 1 2 1)) '(1 1 2 2))
(deftest 5 isort "already sorted" (isort '(1 2 3)) '(1 2 3))

;;; ============================================================
;;; Q6 cons cells / Lab 5 ADT.  Builders use allocate-cell directly.
;;; ============================================================
(deftest 6 my-last "last of (10 20 30)" (my-last (build-adt '(10 20 30))) 30)
(deftest 6 my-last "single element" (my-last (build-adt '(7))) 7)
(deftest 6 my-last "empty list => NIL" (my-last (build-adt '())) nil)

(deftest 6 my-sum "sum of (1 2 3 4)" (my-sum (build-adt '(1 2 3 4))) 10)
(deftest 6 my-sum "empty => 0" (my-sum (build-adt '())) 0)
(deftest 6 my-sum "negatives" (my-sum (build-adt '(5 -5 10))) 10)
(deftest 6 my-sum "single" (my-sum (build-adt '(42))) 42)

(deftest 6 my-reverse-list "returns a list whose cells read reversed"
  (let ((l (build-adt '(1 2 3)))) (adt->list (my-reverse-list l))) '(3 2 1))
(deftest 6 my-reverse-list "size field is correct"
  (let ((l (build-adt '(1 2 3)))) (aref (my-reverse-list l) 1)) 3)
(deftest 6 my-reverse-list "empty list"
  (let ((l (build-adt '()))) (adt->list (my-reverse-list l))) nil)
(deftest 6 my-reverse-list "returns a NEW list vector (not the same object)"
  (let ((l (build-adt '(1 2)))) (eq l (my-reverse-list l))) nil :test eq)

(deftest 6 my-append-end "append to (1 2 3)"
  (let ((l (build-adt '(1 2 3)))) (my-append-end 4 l) (adt->list l)) '(1 2 3 4))
(deftest 6 my-append-end "size incremented"
  (let ((l (build-adt '(1 2 3)))) (my-append-end 4 l) (aref l 1)) 4)
(deftest 6 my-append-end "works on EMPTY list"
  (let ((l (build-adt '()))) (my-append-end 9 l) (adt->list l)) '(9))
(deftest 6 my-append-end "empty list: size becomes 1"
  (let ((l (build-adt '()))) (my-append-end 9 l) (aref l 1)) 1)
(deftest 6 my-append-end "two appends keep order"
  (let ((l (build-adt '(1)))) (my-append-end 2 l) (my-append-end 3 l) (adt->list l)) '(1 2 3))
(deftest 6 my-append-end "returns the list vector"
  (let ((l (build-adt '(1)))) (eq l (my-append-end 2 l))) t :test eq)

;;; ---- run everything automatically when the file is loaded ----
(run-all)