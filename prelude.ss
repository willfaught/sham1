(module prelude mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define prelude:error
    (lambda (s) (error (string-append "*** Exception: " (list->string (prelude:strict s))))))
  
  (define prelude:int-add (lambda (x) (lambda (y) (+ (force x) (force y)))))
  
  (define prelude:int-subtract (lambda (x) (lambda (y) (- (force x) (force y)))))
  
  (define prelude:int-multiply (lambda (x) (lambda (y) (* (force x) (force y)))))
  
  (define prelude:int-divide (lambda (x) (lambda (y) (/ (force x) (force y)))))
  
  #;(define prelude:int-equal (lambda (x) (lambda (y) (equal? (force x) (force y)))))
  
  #;(define prelude:int-not-equal (lambda (x) (lambda (y) (not (equal? (force x) (force y))))))
  
  (define prelude:list-cons (lambda (h) (lambda (t) (cons-immutable h t))))
  
  (define prelude:list-head (lambda (l) (force (car (force l)))))
  
  (define prelude:list-tail (lambda (l) (force (cdr (force l)))))
  
  (define prelude:list-null (lambda (l) (null? (force l))))
  
  (define (prelude:strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (prelude:strict (car value)) (prelude:strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (prelude:strict x)) value))
            (else value))))
  
  #;(define prelude:trace
      (lambda (v)
        (lambda (r) 
          (define (print-list l)
            (display " ")
            (print ((force prelude:list-head) (delay (force l))))
            (if ((force prelude:boolean-not) (delay ((force prelude:list-null) (delay ((force prelude:list-tail) (delay (force l)))))))
                (print-list (delay ((force prelude:list-tail) (delay (force l)))))))
          (cond ((boolean? (force v))
                 (print (force v)) (force r))
                ((char? (force v))
                 (print (force v)) (force r))
                ((number? (force v))
                 (print (force v)) (force r))
                ((or (pair? (force v)) (list? (force v)))
                 (display "(")
                 (if ((force prelude:boolean-not) (delay ((force prelude:list-null) (delay (force v)))))
                     (begin (print ((force prelude:list-head) (delay (force v))))
                            (if ((force prelude:boolean-not) (delay ((force prelude:list-null) (delay ((force prelude:list-tail) (delay (force v)))))))
                                (print-list (delay ((force prelude:list-tail) (delay (force v))))))))
                 (display ")")
                 (force r))))))
  
  (define prelude:tuple-first (lambda (t) (force (vector-ref (force t) 0))))
  
  (define prelude:tuple-second (lambda (t) (force (vector-ref (force t) 1)))))