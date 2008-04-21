(module list mzscheme
  (provide (all-defined))
  
  (define (list-drop x n)
    (if (not (list? x))
        (error 'list-drop "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'list-drop "invalid list length"))
    (if (equal? n 0)
        x
        (list-drop (cdr x) (- n 1))))
  
  (define (list-take x n)
    (if (not (list? x))
        (error 'list-take "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'list-take "invalid list length"))
    (if (equal? n 0)
        null
        (cons (car x) (list-take (cdr x) (- n 1))))))