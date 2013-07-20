;----- print list recursivly -----------;
(defun prnt_list_reverse(lst)
  (unless (null lst)    
    (progn
      (prnt_list_reverse (cdr lst))
      (format t "~A  " (car lst)))))

;CL-USER> (prnt_list_reverse '(1 2 3 4 5 6 7 8 9))
;9  8  7  6  5  4  3  2  1 

;----- cptr 3 - qn2 -----------;
;; the input lists should be sets
(defun my-union(llst rlst)
  (if(null llst) rlst
     (get-union llst rlst)))

(defun get-union(llst rlst) 
  (if(null rlst) llst
     (if(null (member (car rlst) llst))	
	(get-union (append llst (list(car rlst))) (cdr rlst))
	(get-union llst (cdr rlst)))))

;----- cptr 3 - qn8 --------------;
;; Deepaks solution

(defun show-dots (list)
  (if (null list)
      (format t "nil)")
      (progn
	(format t "(~A . " (car list))
	(show-dots (cdr list))
	(format t ")"))))

;CL-USER> (show-dots '(a b c))
;(A . (B . (C . nil))))

;----- shortest path fun from book  --------------;
(defun shortest-path(start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (format t "~% Queue ~A" queue)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end 
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n) 
	      (cons n path))
	  (cdr (assoc node net))))

;----- cptr 3 - qn 9 --------------;
(defun longest-path (start net)
  (lbfs (list (list start)) net nil))

(defun lbfs (queue net long-paths)
  (format t "~%Queue is ~A || ~A" queue long-paths)
  (let ((path (car queue)))
    (let ((node (car path)))
      (if (null queue)
	  (print-longest-paths long-paths)			  
	  (progn
	    (if (null (assoc node net))
		(setf long-paths (process-paths queue path long-paths))
		(setf long-paths nil))
	    (lbfs (append (cdr queue) (new-paths path node net))
		  net long-paths))))))

(defun process-paths (queue path long-paths)
  (remove path queue)
  (if (> (length path) (length (car long-paths)))
      (setf long-paths nil))
  (append (list path) long-paths))

(defun print-longest-paths (long-paths)
  (format t "~%~%Longest paths are ~A"
	  (mapcar #'(lambda (n) 
		      (reverse n)) 
		  long-paths)))
;;; OUTPUT ;;;
;CL-USER> (setf net '((a b c) (b c f) (c d e) (d) (e d h g) (f)))
;((A B C) (B C F) (C D E) (D) (E D H G) (F))

;CL-USER> (longest-path 'a net)

;Queue is ((A))
;Queue is ((B A) (C A))
;Queue is ((C A) (C B A) (F B A))
;Queue is ((C B A) (F B A) (D C A) (E C A))
;Queue is ((F B A) (D C A) (E C A) (D C B A) (E C B A))
;Queue is ((D C A) (E C A) (D C B A) (E C B A))
;Queue is ((E C A) (D C B A) (E C B A))
;Queue is ((D C B A) (E C B A) (D E C A) (H E C A) (G E C A))
;Queue is ((E C B A) (D E C A) (H E C A) (G E C A))
;Queue is ((D E C A) (H E C A) (G E C A) (D E C B A) (H E C B A) (G E C B A))
;Queue is ((H E C A) (G E C A) (D E C B A) (H E C B A) (G E C B A))
;Queue is ((G E C A) (D E C B A) (H E C B A) (G E C B A))
;Queue is ((D E C B A) (H E C B A) (G E C B A))
;Queue is ((H E C B A) (G E C B A))
;Queue is ((G E C B A))
;Queue is NIL

;Longest paths are ((A B C E H) (A B C E G))

;-------------------;

(defun second-word(str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))

;CL-USER> (second-word "mahesh pandurang jadhav")
;"pandurang"

;-------remove-duplicate function pg 66------------;

(defun call-remove-duplicate(pstr)
  (my-remove-duplicate pstr 0))

(defun my-remove-duplicate (str pos)
  (let ((len (length str)))
    (if (or (null str) (equal 1 len) (equal (+ pos 1) len))
	str
	(if (not (equal pos (position (char str pos) str :from-end t)))	      
	    (my-remove-duplicate 
	     (concatenate 'string (subseq str 0 pos) (subseq str (+ pos 1) len)) 
	     pos)
	    (my-remove-duplicate 
	     str
	     (+ pos 1))))))

;CL-USER> (call-remove-duplicate "abracadabra")

;1  abracadabra
;1  bracadabra
;1  racadabra
;1  acadabra
;2  cadabra
;1  cadabra
;2  cdabra
;1  cdabra
;2  cdbra
;2  cdbra
;"cdbra"
   

(defun my-remove-duplicate-2 (str)
  (let ((stack) (result))
    (dotimes (pos (length str))
      (pushnew (char (reverse str) pos) stack))
    (dolist (obj stack)
      (setf result (concatenate 'string result (string (pop stack)))))
    result))


;--------------------------------;	 

; 2.8.a (30);

(defun print-dots (int)
  (do ((i 1 (+ i 1)))
      ((> i int) nil)
	(format t ". ")))

(defun rs-print-dots (int)
  (unless (zerop int)
    (progn
      (rs-print-dots (- int 1)))
    (format t ". ")))

;--------------------------------;

; 2.8.b (30)

(defun init-count-a (lst)
  (if (listp lst)
      (count-a lst 0)))

(defun count-a (lst x)
  (format t "~A ~A~%" lst x)
  (if (null lst)                    ; stop
      (format t "~A" x)             ; return
    (if (eql (car lst) 'a)          ; small step 
	(count-a (cdr lst) (+ 1 x)) ; rest
      (count-a (cdr lst) x))))

;--------------------------------;  

;GCL 8.1

(defun anyoddp (lst)
  (cond ((null lst) nil)
	((oddp (car lst)) t)
	(t (anyoddp (cdr lst)))))

;GCL 8.2
(defun anyoddp-if (lst)
  (if (null lst) nil
    (if (oddp (car lst)) t
      (anyoddp-if (cdr lst)))))

;GCL 8.4

(defun laugh (n)
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (- n 1))))))

;GCL 8.5

(defun addup (lst)
  (cond ((null lst) 0)
	((eq (cdr lst) nil) lst)
	(t (addup-n lst))))

(defun addup-n (lst)
  (if (null (cdr lst)) (car lst)
    (+ (car lst) (addup-n (cdr lst)))))
    
; GCL 8.6

(defun alloddp (lst)
  (cond ((null lst) nil)
	(t (alloddp-n lst))))


(defun alloddp-n (lst)
  (cond ((null lst) t)
	((oddp (car lst)) (alloddp-n (cdr lst)))
	(t 'even)))

; GCL 8.9

(defun rec-nth (n lst)
  (if (null lst) nil
    (rec-nth2 0 n lst)))

(defun rec-nth2 (c n lst)
  (if (null lst) nil
    (if (eq c n) (car lst)
      (rec-nth2 (+ 1 c) n (cdr lst)))))

;GCL 8.10

(defun add1 (x)
  (+ x 1))

(defun sub1 (y)
  (- y 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
	(t (rec-plus (add1 x) (sub1 y)))))

;GCL 8.10.n

(defun rec-plus-n (lst)
  (if (null (cdr lst)) (car lst)
    (rec-plus-n 
     (cons 
      (setf (car (cdr lst)) 
	    (rec-plus (car lst) (car (cdr lst))))
      (cdr (cdr lst))))))

;; given an array having 1 duplicate value find duplicate value

;; (find-duplicate '(1 8 3 9 0 22 66 29 200 17 100 33 88 23 27 31 3))
;; (find-duplicate '(1 2 3 4 5 6))
;; (find-duplicate '())

(defun find-duplicate (arr)
  (if (null arr)
      nil
      (let ((sorted (sort arr #'<)))
	(let ((rst (cdr sorted)))
	  (recurse (car sorted) rst rst)))))

(defun recurse (ptr arr oarr)
  (if (null oarr)
      nil
      (let ((rst (cdr oarr)))
	(if (null arr)
	    (recurse (car oarr) rst rst)
	    (if (= ptr (car arr))
		ptr
		(if (< ptr (car arr))
		    (recurse ptr (cdr arr) oarr)
		    (recurse (car oarr) rst rst)))))))
