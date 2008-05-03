(module prelude mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define (strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (strict (car value)) (strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (strict x)) value))
            (else value))))
  
  (define haskell:error
    (delay (lambda (s) (error (string-append "*** Exception: " (list->string (strict s)))))))
  
  (define haskell:trace
    (delay (lambda (v)
             (lambda (r) 
               (define (print-list l)
                 (display " ")
                 (print ((force haskell:list-head) (delay (force l))))
                 (if ((force haskell:boolean-not) (delay ((force haskell:list-null) (delay ((force haskell:list-tail) (delay (force l)))))))
                     (print-list (delay ((force haskell:list-tail) (delay (force l)))))))
               (cond ((boolean? (force v))
                      (print (force v)) (force r))
                     ((char? (force v))
                      (print (force v)) (force r))
                     ((number? (force v))
                      (print (force v)) (force r))
                     ((or (pair? (force v)) (list? (force v)))
                      (display "(")
                      (if ((force haskell:boolean-not) (delay ((force haskell:list-null) (delay (force v)))))
                          (begin (print ((force haskell:list-head) (delay (force v))))
                                 (if ((force haskell:boolean-not) (delay ((force haskell:list-null) (delay ((force haskell:list-tail) (delay (force v)))))))
                                     (print-list (delay ((force haskell:list-tail) (delay (force v))))))))
                      (display ")")
                      (force r)))))))
  
  (define haskell:int-add (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define haskell:int-subtract (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  (define haskell:int-multiply (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define haskell:int-divide (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define haskell:int-equal (delay (lambda (x) (lambda (y) (equal? (force x) (force y))))))
  
  (define haskell:int-not-equal (delay (lambda (x) (lambda (y) (not (equal? (force x) (force y)))))))
  
  (define haskell:list-cons (delay (lambda (h) (lambda (t) (cons h t)))))
  
  (define haskell:list-head (delay (lambda (l) (force (car (force l))))))
  
  (define haskell:list-tail (delay (lambda (l) (force (cdr (force l))))))
  
  (define haskell:list-null (delay (lambda (l) (null? (force l)))))
  
  (define haskell:tuple-first (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define haskell:tuple-second (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  (define haskell:boolean-true (delay #t))
  
  (define haskell:boolean-false (delay #f))
  
  (define haskell:boolean-and (delay (lambda (x) (lambda (y) (and (force x) (force y))))))
  
  (define haskell:boolean-or (delay (lambda (x) (lambda (y) (or (force x) (force y))))))
  
  (define haskell:boolean-not (delay (lambda (x) (not (force x))))))