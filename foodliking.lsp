;; Facts
(defvar *delicious* '(cake pickles biryani))
(defvar *spicy* '(pickles))

;; Rules
(defun likes (person food)
  (cond
    ((and (eq person 'priya) (member food *delicious*)) t)
    ((and (eq person 'prakash) (member food *spicy*) (member food *delicious*)) t)
    (t nil)))

;; Queries
(print (likes 'priya 'cake))      ; T
(print (likes 'priya 'coffee))     ; NIL
(print (likes 'prakash 'pickles))  ; T
(print (likes 'prakash 'biryani))  ; NIL