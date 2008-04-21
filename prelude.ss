(module prelude mzscheme
  (require (lib "types.ss" "(lib "match.ss")")
           (lib "match.ss")
           (lib "43" "srfi"))
  
  (provide (all-defined))
  
  (define prelude-types
    (make-immutable-hash-table `(("error" . ,(let ((t (fresh-type-variable)))
                                             (make-universal-type (list t) (make-function-type (list (make-list-type (make-character-type)) t)))))
                                 ("trace" . ,(let ((t1 (fresh-type-variable))
                                                   (t2 (fresh-type-variable)))
                                             (make-universal-type (list t1 t2) (make-function-type (list t1 t2 t2)))))
                                 ("+" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("-" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("*" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("/" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("==" . ,(let ((t (fresh-type-variable)))
                                            (make-universal-type (list t) (make-function-type (list t t (make-boolean-type))))))
                                 ("/=" . ,(let ((t (fresh-type-variable)))
                                            (make-function-type (list t t (make-boolean-type)))))
                                 (":" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list t (make-list-type t) (make-list-type t))))))
                                 ("head" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) t)))))
                                 ("tail" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) (make-list-type t))))))
                                 ("null" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) (make-boolean-type))))))
                                 ("fst" . ,(let ((t1 (fresh-type-variable))
                                               (t2 (fresh-type-variable)))
                                         (make-universal-type (list t1 t2) (make-function-type (list (make-tuple-type (list t1 t2)) t1)))))
                                 ("snd" . ,(let ((t1 (fresh-type-variable))
                                               (t2 (fresh-type-variable)))
                                         (make-universal-type (list t1 t2) (make-function-type (list (make-tuple-type (list t1 t2)) t2)))))
                                 ("True" . ,(make-boolean-type))
                                 ("False" . ,(make-boolean-type))
                                 ("&&" . ,(make-function-type (list (make-boolean-type) (make-boolean-type) (make-boolean-type))))
                                 ("||" . ,(make-function-type (list (make-boolean-type) (make-boolean-type) (make-boolean-type))))
                                 ("not" . ,(make-function-type (list (make-boolean-type) (make-boolean-type))))) 'equal))
                                 
  
  (define prelude-declarations '("error" "trace" "+" "-" "*" "/" "==" "/=" ":" "head" "tail" "null" "fst" "snd" "True" "False" "&&" "||" "not"))
  
  (define force-list (match-lambda ((h . t) (cons (force h) (force-list (force t))))
                                   (() null)))
  
  (define haskell:error
    (delay (lambda (s) (error (string-append "*** Exception: " (list->string (force-list (force s))))))))
  
  (define haskell:trace
    (delay (lambda (v)
             (lambda (r) 
               (define (print-list l)
                 (display " ")
                 (print ((force haskell:head) (delay (force l))))
                 (if ((force haskell:not) (delay ((force haskell:null) (delay ((force haskell:tail) (delay (force l)))))))
                     (print-list (delay ((force haskell:tail) (delay (force l)))))))
               (cond ((boolean? (force v)) (print (force v)) (force r))
                     ((char? (force v)) (print (force v)) (force r))
                     ((number? (force v)) (print (force v)) (force r))
                     ((or (pair? (force v)) (list? (force v))) (display "(")
                                                               (if ((force haskell:not) (delay ((force haskell:null) (delay (force v)))))
                                                                   (begin (print ((force haskell:head) (delay (force v))))
                                                                          (if ((force haskell:not) (delay ((force haskell:null) (delay ((force haskell:tail) (delay (force v)))))))
                                                                              (print-list (delay ((force haskell:tail) (delay (force v))))))))
                                                               (display ")")
                                                               (force r)))))))
  
  (define haskell:+ (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define haskell:- (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  (define haskell:* (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define haskell:/ (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define haskell:== (delay (lambda (x) (lambda (y) (equal? (force x) (force y))))))
  
  (define haskell:/= (delay (lambda (x) (lambda (y) (not (equal? (force x) (force y)))))))
  
  (define haskell:: (delay (lambda (h) (lambda (t) (cons h t)))))
  
  (define haskell:head (delay (lambda (l) (force (car (force l))))))
  
  (define haskell:tail (delay (lambda (l) (force (cdr (force l))))))
  
  (define haskell:null (delay (lambda (l) (null? (force l)))))
  
  (define haskell:fst (delay (lambda (t) (vector-ref (force t) 0))))
  
  (define haskell:snd (delay (lambda (t) (vector-ref (force t) 1))))
  
  (define haskell:True (delay #t))
  
  (define haskell:False (delay #f))
  
  (define haskell:&& (delay (lambda (x) (lambda (y) (and (force x) (force y))))))
  
  (define haskell:\|\| (delay (lambda (x) (lambda (y) (or (force x) (force y))))))
  
  (define haskell:not (delay (lambda (x) (not (force x))))))
