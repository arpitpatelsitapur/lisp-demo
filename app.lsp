(format t "Hello, World!")
(print "LISP is fun.")


;; Mutable objects
(print (atom 20.3))
(print (list 1 2 3))

(let ((message "Hello, LISP!")) ;; local variables
  (print message))

(print message) ;; This will cause an error because message is not defined outside the let block

(defparameter *greeting* "Hello, LISP!") ;; global variables
(defvar *farewell* "Goodbye, LISP!") 

(print *greeting*)
(print *farewell*)
 
;; Immutable objects
(defconstant MY-PI 3.14159)
(print MY-PI)

;; Immutable Functions
(defun circumference (x)
  "Return the circumference of circle."
  (* 2 MY-PI x))
(print (circumference 5))

(defun area (x)
  "Return the area of circle."
  (* MY-PI x x))
(print (area 5))

;; HOF(higher order functions)
(print (mapcar #'circumference (list 5 10 20)))

(loop for x from 0 to 9 by 3 do
    (print x))

;; running 
;; sbcl --script app.lsp
