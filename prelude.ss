(module prelude mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define (strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (strict (car value)) (strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (strict x)) value))
            (else value))))
  
  (define prelude:error
    (delay (lambda (s) (error (string-append "*** Exception: " (list->string (strict s)))))))
  
  (define prelude:trace
    (delay (lambda (v)
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
                      (force r)))))))
  
  (define prelude:int-add (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define prelude:int-subtract (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  (define prelude:int-multiply (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define prelude:int-divide (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define prelude:int-equal (delay (lambda (x) (lambda (y) (equal? (force x) (force y))))))
  
  (define prelude:int-not-equal (delay (lambda (x) (lambda (y) (not (equal? (force x) (force y)))))))
  
  (define prelude:list-cons (delay (lambda (h) (lambda (t) (cons h t)))))
  
  (define prelude:list-head (delay (lambda (l) (force (car (force l))))))
  
  (define prelude:list-tail (delay (lambda (l) (force (cdr (force l))))))
  
  (define prelude:list-null (delay (lambda (l) (null? (force l)))))
  
  (define prelude:tuple-first (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define prelude:tuple-second (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  (define prelude:boolean-true (delay #t))
  
  (define prelude:boolean-false (delay #f))
  
  (define prelude:boolean-and (delay (lambda (x) (lambda (y) (and (force x) (force y))))))
  
  (define prelude:boolean-or (delay (lambda (x) (lambda (y) (or (force x) (force y))))))
  
  (define prelude:boolean-not (delay (lambda (x) (not (force x))))))