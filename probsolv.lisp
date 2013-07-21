
;; given an array having 1 duplicate value find duplicate value

;; (find-duplicate '(1 8 3 9 0 22 66 29 200 17 100 33 88 23 27 31 3))
;; (find-duplicate '(1 2 3 4 5 6))
;; (find-duplicate '())

(defun find-duplicate (arr)
  (if (null arr)
      (format t "Provide values!~%")
      (let ((sorted (sort arr #'<)))
	(let ((rst (cdr sorted)))
	  (recurse (car sorted) rst rst)))))

(defun recurse (ptr arr oarr)
  (if (null oarr)
      (format t "No Duplicate!")
      (let ((rst (cdr oarr)))
	(if (null arr)
	    (recurse (car oarr) rst rst)
	    (if (= ptr (car arr))
		(format t "Duplicate value is ~A~%" ptr)
		(if (< ptr (car arr))
		    (recurse ptr (cdr arr) oarr)
		    (recurse (car oarr) rst rst)))))))

;; Find the missing number - 1 to N

;; (find-missing-number '(1 2 3 4 6 7 8 9))
;; (find-missing-number '(1 2 3 4 5 6 7 9 10 11 12 13 14))

(defun find-missing-number (lst)
  (if (null lst)
      (format t "Provide List ~%")
      (missing-num-algo lst)))

(defun missing-num-algo (lst)
  (- (sum-cons-num (+ (length lst) 1)) (reduce #'+ lst)))

(defun sum-cons-num (n)
  (/ (* n (+ n 1 )) 2))

;;Given a string, print all its permutations
;;How to check if a linked list is a palindrome? 
;;Given two lists how will you find the common elements between them?

;;Given an unsorted array and a number a, find a pair of numbers(b,c) from the arraysch that a=b+c.

(defun find-pair (a lst)
  (if (null lst)
      (format t "Provide list")
      (rec-pair a (car lst) (cdr lst) (cdr lst))))

(defun rec-pair (a b lst olst)
  ;(format t "~A ~A ~A ~A~%" a b lst olst)
  (if (null olst)
      (format t "done searching!")
      (if (null lst)
	  (rec-pair a (car olst) (cdr olst) (cdr olst))
	  (if (= a (+ b (car lst)))
	      (progn 
		(format t "pair for ~A is (~A, ~A)~%" a b (car lst))
		(rec-pair a (car olst) (cdr olst) (cdr olst)))
	      (rec-pair a b (cdr lst) olst)))))
