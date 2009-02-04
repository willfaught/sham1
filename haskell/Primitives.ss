(module Primitives mzscheme
  (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
           (only (lib "43.ss" "srfi") vector-map)
           (only (lib "list.ss") foldl foldr)
           (only (lib "match.ss") match)
           (lib "Types.ss" "sham"))
  
  (provide (all-defined))
  
  (define-struct haskell/type/Bool () #f)
  
  (define-struct (haskell/constructor/False haskell/type/Bool) () #f)
  
  (define-struct (haskell/constructor/True haskell/type/Bool) () #f)
  
  (define-struct haskell/type/#List () #f)
  
  (define-struct (haskell/constructor/#Nil haskell/type/#List) () #f)
  
  (define-struct (haskell/constructor/#Cons haskell/type/#List) (head tail) #f)
  
  (define-struct haskell/type/#Unit () #f)
  
  (define-struct (haskell/constructor/#Unit haskell/type/#Unit) () #f)
  
  (define haskell/error
    (lambda (x) (error (string-append "*** Exception: " "TODO"))))
  
  (define haskell/fst
    (delay (lambda (t) (force (vector-ref (force t) 0)))))
  
  (define haskell/head
    (delay (lambda (x) (force (haskell/constructor/#Cons-head (force x))))))
  
  (define haskell/isFalse
    (delay (lambda (x) (if (haskell/constructor/False? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/isTrue
    (delay (lambda (x) (if (haskell/constructor/True? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/null
    (delay (lambda (x) (if (haskell/constructor/#Nil? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/snd
    (delay (lambda (t) (force (vector-ref (force t) 1)))))
  
  (define haskell/tail
    (delay (lambda (x) (force (haskell/constructor/#Cons-tail (force x))))))
  
  #;(define haskell/trace
      (delay (lambda (x) (lambda (y) (print ((force primitive/show) x)) (force y)))))
  
  (define haskell/False
    (delay (make-haskell/constructor/False)))
  
  (define haskell/True
    (delay (make-haskell/constructor/True)))
  
  (define haskell/:
    (delay (lambda (x) (lambda (y) (make-haskell/constructor/#Cons x y)))))
  
  (define primitive/equal (delay (lambda (x) (lambda (y) (if (equal? (force x) (force y))
                                                             (force haskell/True)
                                                             (force haskell/False))))))
  
  (define primitive/numberAdd (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define primitive/numberDivide (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define primitive/numberMultiply (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define primitive/numberSubtract (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  #;(define primitive/show 'TODO)
  
  #;(define haskell/strict 'TODO)
  
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