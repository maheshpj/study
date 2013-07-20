
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
