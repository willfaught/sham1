(module scheme mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "types.ss" "haskell")
           (lib "match.ss"))
  
  (provide (all-defined))
  
  (define (strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (strict (car value)) (strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (strict x)) value))
            (else value))))
  
  (define force-list (match-lambda ((h . t) (cons (force h) (force-list (force t))))
                                   (() null)))
  
  (define prelude:error
    (delay (lambda (s) (error (string-append "*** Exception: " (list->string (force-list (force s))))))))
  
  (define prelude:trace
    (delay (lambda (v)
             (lambda (r) 
               (define (print-list l)
                 (display " ")
                 (print ((force prelude:head) (delay (force l))))
                 (if ((force prelude:not) (delay ((force prelude:null) (delay ((force prelude:tail) (delay (force l)))))))
                     (print-list (delay ((force prelude:tail) (delay (force l)))))))
               (cond ((boolean? (force v)) (print (force v)) (force r))
                     ((char? (force v)) (print (force v)) (force r))
                     ((number? (force v)) (print (force v)) (force r))
                     ((or (pair? (force v)) (list? (force v))) (display "(")
                                                               (if ((force prelude:not) (delay ((force prelude:null) (delay (force v)))))
                                                                   (begin (print ((force prelude:head) (delay (force v))))
                                                                          (if ((force prelude:not) (delay ((force prelude:null) (delay ((force prelude:tail) (delay (force v)))))))
                                                                              (print-list (delay ((force prelude:tail) (delay (force v))))))))
                                                               (display ")")
                                                               (force r)))))))
  
  (define prelude:+ (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define prelude:- (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  (define prelude:* (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define prelude:/ (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define prelude:== (delay (lambda (x) (lambda (y) (equal? (force x) (force y))))))
  
  (define prelude:/= (delay (lambda (x) (lambda (y) (not (equal? (force x) (force y)))))))
  
  (define prelude:: (delay (lambda (h) (lambda (t) (cons h t)))))
  
  (define prelude:head (delay (lambda (l) (force (car (force l))))))
  
  (define prelude:tail (delay (lambda (l) (force (cdr (force l))))))
  
  (define prelude:null (delay (lambda (l) (null? (force l)))))
  
  (define prelude:fst (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define prelude:snd (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  (define prelude:True (delay #t))
  
  (define prelude:False (delay #f))
  
  (define prelude:&& (delay (lambda (x) (lambda (y) (and (force x) (force y))))))
  
  (define prelude:\|\| (delay (lambda (x) (lambda (y) (or (force x) (force y))))))
  
  (define prelude:not (delay (lambda (x) (not (force x))))))