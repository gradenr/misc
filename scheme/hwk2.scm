;Graden Rea
;Artificial Intelligence Homework 2
;4/13/11
;
;swap-adjacent switches every two elements in a list
(define (swap-adjacent alist) 
  (cond ((null? alist) '() )
  	((null? (cdr alist)) alist)
	(#t (let ((first  (car alist))
	 	  (second (car (cdr alist)))
	     	  (rest   (cdr (cdr alist))))
	      (cons second (cons first (swap-adjacent rest)))))))

;rotate takes the element in the front of the list and appends it to the 
;end n times
(define (rotate-left n alist)
  (if (equal? n 0)
      alist
      (rotate-left (- n 1) (rotate-left-once alist))))

;rotates a list left once
(define (rotate-left-once alist)
  (if (null? alist)
       alist
      (let ((tail (cdr  alist))
            (head (list (car alist))))
         (append tail head))))

;a palindrome is a list that is the same backwards and forwards
(define (palindrome? alist) (equal? alist (reverse-list alist)))

;The built-in function reverse would work here, but it was not on the list of
;allowed functions for this assignment
;
;reverse returns a list with all the elements in the opposite order
;it is really just a nice front end of reverse-helper
;reverse is tail recursive
(define (reverse-list alist) (reverse-helper alist '() ))

(define (reverse-helper worklist accum)
  (if (null? worklist) 
      accum
      (reverse-helper (cdr worklist) (cons (car worklist) accum))))
