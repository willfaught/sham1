(module Primitives mzscheme
  (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
           (only (lib "43.ss" "srfi") vector-map)
           (lib "contract.ss")
           (only (lib "list.ss") foldl foldr)
           (lib "match.ss")
           (lib "Types.ss" "sham"))
  
  (provide (all-defined))
  
  (define-struct haskell/type/Bool () #f)
  
  (define-struct (haskell/constructor/False haskell/type/Bool) () #f)
  
  (define haskell/False
    (delay (make-haskell/constructor/False)))
  
  (define haskell/isFalse
    (delay (lambda (x) (if (haskell/constructor/False? (force x))
                           (force (haskell/True))
                           (force (haskell/False))))))
  
  (define-struct (haskell/constructor/True haskell/type/Bool) () #f)
  
  (define haskell/True
    (delay (make-haskell/constructor/True)))
  
  (define haskell/isTrue
    (delay (lambda (x) (if (haskell/constructor/True? (force x))
                           (force (haskell/True))
                           (force (haskell/False))))))
  
  (define-struct haskell/type/#List () #f)
  
  (define-struct (haskell/constructor/#Empty haskell/type/#List) () #f)
  
  (define haskell:null
    (delay (lambda (x) (if (haskell/constructor/#Empty? (force x))
                           (force (haskell/True))
                           (force (haskell/False))))))
  
  (define-struct (haskell/constructor/#Cons haskell/type/#List) (head tail) #f)
  
  (define haskell::
    (delay (lambda (x) (lambda (y) (make-haskell/constructor/#Cons x y)))))
  
  (define haskell:head
    (delay (lambda (x) (force (haskell/constructor/#Cons-head x)))))
  
  (define haskell:tail
    (delay (lambda (x) (force (haskell/constructor/#Cons-tail x)))))
  
  (define-struct haskell/type/#Unit () #f)
  
  (define-struct (haskell/constructor/#Unit haskell/type/#Unit) () #f)
  
  (define haskell:fst
    (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define haskell:snd
    (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  #;(define primitive:error
      (lambda (s) (error (string-append "*** Exception: " (list->string (primitive:strict s))))))
  
  (define primitive:numberAdd (lambda (x) (lambda (y) (+ (force x) (force y)))))
  
  (define primitive:numberSubtract (lambda (x) (lambda (y) (- (force x) (force y)))))
  
  (define primitive:numberMultiply (lambda (x) (lambda (y) (* (force x) (force y)))))
  
  (define primitive:numberDivide (lambda (x) (lambda (y) (/ (force x) (force y)))))
  
  (define primitive:equal (lambda (x) (lambda (y) (if (equal? (force x) (force y))
                                                      (force haskell/True)
                                                      (force haskell/False)))))
  
  #;(define primitive:not-equal (lambda (x) (lambda (y) (not (equal? (force x) (force y))))))
  
  #;(define (primitive:strict term)
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
  #;(define (primitive:type-less-general? x y)
      (match (list x y)
        (((? type-variable? _) (? type-variable? _)) #t)
        (((? (lambda (x) (not (type-variable? x)))) (? type-variable? _)) #t)
        (((? type-variable? _) (? (lambda (x) (not (type-variable? x))))) #f)
        ((($ function-type p1 r1) ($ function-type p2 r2)) (and (primitive:type-less-general? p1 p2)
                                                                (primitive:type-less-general? r1 r2)))
        ((($ tuple-type t1) ($ tuple-type t2)) (and (equal? (length t1) (length t2))
                                                    (foldl (lambda (x y) (and x y)) #t (map primitive:type-less-general? t1 t2))))
        ((x y) (equal? x y)))))