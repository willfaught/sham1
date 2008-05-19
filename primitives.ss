(module primitives mzscheme
  (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
           (only (lib "43.ss" "srfi") vector-map)
           (lib "contract.ss")
           (only (lib "list.ss") foldl foldr)
           (lib "match.ss")
           (lib "types.ss" "haskell"))
  
  (provide (all-defined))
  
  (define-struct haskell-type:Bool () #f)
  
  (define-struct (haskell-constructor:False haskell-type:Bool) () #f)
  
  (define-struct (haskell-constructor:True haskell-type:Bool) () #f)
  
  (define haskell:fst (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define haskell:head (delay (lambda (l) (force (car (force l))))))
  
  (define haskell:isFalse (delay (lambda (x) (if (haskell-constructor:False? (force x)) (make-haskell-constructor:True) (make-haskell-constructor:False)))))
  
  (define haskell:isTrue (delay (lambda (x) (if (haskell-constructor:True? (force x)) (make-haskell-constructor:True) (make-haskell-constructor:False)))))
  
  (define haskell:null (delay (lambda (l) (null? (force l)))))
  
  (define haskell:snd (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  (define haskell:tail (delay (lambda (l) (force (cdr (force l))))))
  
  (define haskell:False (delay (make-haskell-constructor:False)))
  
  (define haskell:True (delay (make-haskell-constructor:True)))
  
  (define haskell:: (delay (lambda (h) (lambda (t) (cons-immutable h t)))))
  
  (define scheme:fst (delay (contract (-> (vector/c any/c
                                                    any/c)
                                          any/c)
                                      (lambda (x1)
                                        ((force haskell:fst) (delay (let ((x x1))
                                                                      (vector-immutable (delay (vector-ref x
                                                                                                           0))
                                                                                        (delay (vector-ref x
                                                                                                           1)))))))
                                      'haskell
                                      'scheme)))
  
  (define scheme:head (delay (contract
                              (-> (and/c (listof any/c)
                                         (flat-contract proper-list?)
                                         (flat-contract (lambda (x)
                                                          (not (circular-list? x)))))
                                  any/c)
                              (lambda (x1)
                                ((force haskell:head) (delay (foldr (lambda (x y)
                                                                      (cons-immutable (delay x)
                                                                                      (delay y)))
                                                                    null
                                                                    x1))))
                              'haskell
                              'scheme)))
  
  (define scheme:isFalse (delay (contract (-> (flat-contract haskell-type:Bool?)
                                              any/c)
                                          (lambda (x1)
                                            ((force haskell:isFalse) (delay x1)))
                                          'haskell
                                          'scheme)))
  
  (define scheme:isTrue (delay (contract (-> (flat-contract haskell-type:Bool?)
                                             any/c)
                                         (lambda (x1)
                                           ((force haskell:isTrue) (delay x1)))
                                         'haskell
                                         'scheme)))
  
  (define scheme:null (delay (contract
                              (-> (and/c (listof any/c)
                                         (flat-contract proper-list?)
                                         (flat-contract (lambda (x)
                                                          (not (circular-list? x)))))
                                  any/c)
                              (lambda (x1)
                                ((force haskell:null) (delay (foldr (lambda (x y)
                                                                      (cons-immutable (delay x)
                                                                                      (delay y)))
                                                                    null
                                                                    x1))))
                              'haskell
                              'scheme)))
  
  (define scheme:snd (delay (contract (-> (vector/c any/c
                                                    any/c)
                                          any/c)
                                      (lambda (x1)
                                        ((force haskell:snd) (delay (let ((x x1))
                                                                      (vector-immutable (delay (vector-ref x 0))
                                                                                        (delay (vector-ref x 1)))))))
                                      'haskell
                                      'scheme)))
  
  (define scheme:tail (delay (contract
                              (-> (and/c (listof any/c)
                                         (flat-contract proper-list?)
                                         (flat-contract (lambda (x)
                                                          (not (circular-list? x)))))
                                  any/c)
                              (lambda (x1)
                                ((force haskell:tail) (delay (foldr (lambda (x y)
                                                                      (cons-immutable (delay x)
                                                                                      (delay y)))
                                                                    null
                                                                    x1))))
                              'haskell
                              'scheme)))
  
  (define scheme:False (delay (contract any/c
                                        (force haskell:False)
                                        'haskell
                                        'scheme)))
  
  (define scheme:True (delay (contract any/c
                                       (force haskell:True)
                                       'haskell
                                       'scheme)))
  
  (define scheme:: (delay (contract
                           (-> any/c
                               (-> (and/c (listof any/c)
                                          (flat-contract proper-list?)
                                          (flat-contract (lambda (x)
                                                           (not (circular-list? x)))))
                                   any/c))
                           (lambda (x1)
                             (lambda (x2)
                               (((force haskell::) (delay x1)) (delay (foldr (lambda (x y)
                                                                                  (cons-immutable (delay x)
                                                                                                  (delay y)))
                                                                                null
                                                                                x2)))))
                           'haskell
                           'scheme)))
  
  (define primitive:error
    (lambda (s) (error (string-append "*** Exception: " (list->string (primitive:strict s))))))
  
  (define primitive:number-add (lambda (x) (lambda (y) (+ (force x) (force y)))))
  
  (define primitive:number-subtract (lambda (x) (lambda (y) (- (force x) (force y)))))
  
  (define primitive:number-multiply (lambda (x) (lambda (y) (* (force x) (force y)))))
  
  (define primitive:number-divide (lambda (x) (lambda (y) (/ (force x) (force y)))))
  
  (define primitive:equal (lambda (x) (lambda (y) (if (equal? (force x) (force y))
                                                      (force haskell:True)
                                                      (force haskell:False)))))
  
  (define primitive:not-equal (lambda (x) (lambda (y) (not (equal? (force x) (force y))))))
  
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
  
  ; primitive:type-less-general? :: type type -> boolean
  (define (primitive:type-less-general? x y)
    (match (list x y)
      (((? type-variable? _) (? type-variable? _)) #t)
      (((? (lambda (x) (not (type-variable? x)))) (? type-variable? _)) #t)
      (((? type-variable? _) (? (lambda (x) (not (type-variable? x))))) #f)
      ((($ function-type p1 r1) ($ function-type p2 r2)) (and (primitive:type-less-general? p1 p2)
                                                              (primitive:type-less-general? r1 r2)))
      ((($ tuple-type t1) ($ tuple-type t2)) (and (equal? (length t1) (length t2))
                                                  (foldl (lambda (x y) (and x y)) #t (map primitive:type-less-general? t1 t2))))
      ((x y) (equal? x y)))))