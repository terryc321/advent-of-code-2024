

(sort '(1 2 3 2 1 ) #'<)

(defvar p '(3   4
                4   3
                2   5
                1   3
                3   9
                3   3))

;;split them 
(defun split (s)  
  (let ((a (car s))
        (b (cadr s)))
    (split (cddr s))))

(split p)
    

