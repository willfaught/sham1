(module Prelude scheme
  (provide (rename-out (haskell/False False)
                       (haskell/Nil# Nil#)
                       (haskell/True True)
                       (haskell/Unit# Unit#)
                       (haskell/error error)
                       (haskell/fst fst)
                       (haskell/head head)
                       (haskell/isFalse isFalse)
                       (haskell/isTrue isTrue)
                       (haskell/null null)
                       (haskell/snd snd)
                       (haskell/tail tail)
                       (haskell/: :)))
  
  (define-struct haskell/type/Bool () #:transparent)
  
  (define-struct haskell/type/Char () #:transparent)
  
  (define-struct haskell/type/Float () #:transparent)
  
  (define-struct haskell/type/Int () #:transparent)
  
  (define-struct haskell/type/List# () #:transparent)
  
  (define-struct haskell/type/Unit# () #:transparent)
  
  (define-struct (haskell/constructor/Char# haskell/type/Char) (value) #:transparent)
  
  (define-struct (haskell/constructor/Cons# haskell/type/List#) (head tail) #:transparent)
  
  (define-struct (haskell/constructor/False haskell/type/Bool) () #:transparent)
  
  (define-struct (haskell/constructor/Float# haskell/type/Float) (value) #:transparent)
  
  (define-struct (haskell/constructor/Int# haskell/type/Int) (value) #:transparent)
  
  (define-struct (haskell/constructor/Nil# haskell/type/List#) () #:transparent)
  
  (define-struct (haskell/constructor/True haskell/type/Bool) () #:transparent)
  
  (define-struct (haskell/constructor/Unit# haskell/type/Unit#) () #:transparent)
  
  (define haskell/False
    (delay (make-haskell/constructor/False)))
  
  (define haskell/Nil#
    (delay (make-haskell/constructor/Nil#)))
  
  (define haskell/True
    (delay (make-haskell/constructor/True)))
  
  (define haskell/Unit#
    (delay (make-haskell/constructor/Unit#)))
  
  (define haskell/error
    (delay (lambda (x) (error (string-append "*** Exception: " "TODO")))))
  
  (define haskell/fst
    (delay (lambda (t) (force (list-ref (force t) 0)))))
  
  (define haskell/head
    (delay (lambda (x) (force (haskell/constructor/Cons#-head (force x))))))
  
  (define haskell/isFalse
    (delay (lambda (x) (if (haskell/constructor/False? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/isTrue
    (delay (lambda (x) (if (haskell/constructor/True? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/null
    (delay (lambda (x) (if (haskell/constructor/Nil#? (force x))
                           (force haskell/True)
                           (force haskell/False)))))
  
  (define haskell/snd
    (delay (lambda (t) (force (list-ref (force t) 1)))))
  
  (define haskell/tail
    (delay (lambda (x) (force (haskell/constructor/Cons#-tail (force x))))))
  
  (define haskell/:
    (delay (lambda (x) (lambda (y) (make-haskell/constructor/Cons# x y)))))
  
  ;;;;;;;;;
  
  #;(define haskell/trace
      (delay (lambda (x) (lambda (y) (print ((force primitive/show) x)) (force y)))))
  
  (define primitive/equal (delay (lambda (x) (lambda (y) (if (equal? (force x) (force y))
                                                             (force haskell/True)
                                                             (force haskell/False))))))
  
  (define primitive/numberAdd (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define primitive/numberDivide (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define primitive/numberMultiply (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define primitive/numberSubtract (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  #;(define primitive/show 'TODO)
  
  #;(define haskell/strict 'TODO))