(module primitives mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define primitive:error
    (lambda (s) (error (string-append "*** Exception: " (list->string (primitive:strict s))))))
  
  (define primitive:int-add (lambda (x) (lambda (y) (+ (force x) (force y)))))
  
  (define primitive:int-subtract (lambda (x) (lambda (y) (- (force x) (force y)))))
  
  (define primitive:int-multiply (lambda (x) (lambda (y) (* (force x) (force y)))))
  
  (define primitive:int-divide (lambda (x) (lambda (y) (/ (force x) (force y)))))
  
  #;(define primitive:int-equal (lambda (x) (lambda (y) (equal? (force x) (force y)))))
  
  #;(define primitive:int-not-equal (lambda (x) (lambda (y) (not (equal? (force x) (force y))))))
  
  (define primitive:list-cons (lambda (h) (lambda (t) (cons-immutable h t))))
  
  (define primitive:list-head (lambda (l) (force (car (force l)))))
  
  (define primitive:list-tail (lambda (l) (force (cdr (force l)))))
  
  (define primitive:list-null (lambda (l) (null? (force l))))
  
  (define (primitive:strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (primitive:strict (car value)) (primitive:strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (primitive:strict x)) value))
            (else value))))
  
  #;(define primitive:trace
      (lambda (v)
        (lambda (r) 
          (define (print-list l)
            (display " ")
            (print ((force primitive:list-head) (delay (force l))))
            (if ((force primitive:boolean-not) (delay ((force primitive:list-null) (delay ((force primitive:list-tail) (delay (force l)))))))
                (print-list (delay ((force primitive:list-tail) (delay (force l)))))))
          (cond ((boolean? (force v))
                 (print (force v)) (force r))
                ((char? (force v))
                 (print (force v)) (force r))
                ((number? (force v))
                 (print (force v)) (force r))
                ((or (pair? (force v)) (list? (force v)))
                 (display "(")
                 (if ((force primitive:boolean-not) (delay ((force primitive:list-null) (delay (force v)))))
                     (begin (print ((force primitive:list-head) (delay (force v))))
                            (if ((force primitive:boolean-not) (delay ((force primitive:list-null) (delay ((force primitive:list-tail) (delay (force v)))))))
                                (print-list (delay ((force primitive:list-tail) (delay (force v))))))))
                 (display ")")
                 (force r))))))
  
  (define primitive:tuple-first (lambda (t) (force (vector-ref (force t) 0))))
  
  (define primitive:tuple-second (lambda (t) (force (vector-ref (force t) 1)))))