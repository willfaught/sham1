(module Haskell scheme
  (require (lib "Parsing.ss" "sham" "haskell")
           (lib "Primitives.ss" "sham" "haskell"))
  
  (provide type/haskell/Bool
           type/haskell/Char
           type/haskell/Float
           type/haskell/Int
           type/haskell/List#
           type/haskell/Tuple#
           (rename-out (variable/False False)
                       (variable/Nil# Nil#)
                       (variable/True True)
                       (variable/Unit# Unit#)
                       (variable/error error)
                       (variable/fst fst)
                       (variable/head head)
                       (variable/isFalse isFalse)
                       (variable/isTrue isTrue)
                       (variable/null null)
                       (variable/snd snd)
                       (variable/tail tail)
                       (variable/: :)))
  
  (define-contract-struct constructor/Cons# (head tail))
  
  (define-contract-struct constructor/False ())
  
  (define-contract-struct constructor/Nil# ())
  
  (define-contract-struct constructor/True ())
  
  (define-contract-struct constructor/Unit# ())
  
  (define type/haskell/Bool
    (curry (lambda (language value) (contract (or/c (struct/c constructor/False) (struct/c constructor/True)) value language 'haskell))))
  
  (define type/haskell/Char
    (struct/c Char# char?))
  
  (define type/haskell/Float
    (struct/c Float# number?))
  
  (define type/haskell/Int
    (struct/c Int# integer?))
  
  (define (type/haskell/List# haskell/a)
    (recursive-contract (or/c (constructor/Nil#/c) (constructor/Cons#/c (promise/c haskell/a) (promise/c (type/haskell/List# haskell/a))))))
  
  (define (type/haskell/Tuple# haskell/a)
    (eval `(list/c ,@haskell/a)))
  
  (define variable/False
    (delay (make-constructor/False)))
  
  (define variable/Nil#
    (delay (make-constructor/Nil#)))
  
  (define variable/True
    (delay (make-constructor/True)))
  
  (define variable/Unit#
    (delay (make-constructor/Unit#)))
  
  (define variable/error
    (delay (lambda (x) (error (string-append "*** Exception: " "TODO")))))
  
  (define variable/fst
    (delay (lambda (t) (force (list-ref (force t) 0)))))
  
  (define variable/head
    (delay (lambda (x) (force (constructor/Cons#-head (force x))))))
  
  (define variable/isFalse
    (delay (lambda (x) (if (constructor/False? (force x))
                           (force variable/True)
                           (force variable/False)))))
  
  (define variable/isTrue
    (delay (lambda (x) (if (constructor/True? (force x))
                           (force variable/True)
                           (force variable/False)))))
  
  (define variable/null
    (delay (lambda (x) (if (constructor/Nil#? (force x))
                           (force variable/True)
                           (force variable/False)))))
  
  (define variable/snd
    (delay (lambda (t) (force (list-ref (force t) 1)))))
  
  (define variable/tail
    (delay (lambda (x) (force (constructor/Cons#-tail (force x))))))
  
  (define variable/:
    (delay (lambda (x) (lambda (y) (make-constructor/Cons# x y)))))
  
  ;;;;;;;;;
  
  #;(define variable/trace
      (delay (lambda (x) (lambda (y) (print ((force primitive/show) x)) (force y)))))
  
  (define primitive/equal (delay (lambda (x) (lambda (y) (if (equal? (force x) (force y))
                                                             (force variable/True)
                                                             (force variable/False))))))
  
  (define primitive/numberAdd (delay (lambda (x) (lambda (y) (+ (force x) (force y))))))
  
  (define primitive/numberDivide (delay (lambda (x) (lambda (y) (/ (force x) (force y))))))
  
  (define primitive/numberMultiply (delay (lambda (x) (lambda (y) (* (force x) (force y))))))
  
  (define primitive/numberSubtract (delay (lambda (x) (lambda (y) (- (force x) (force y))))))
  
  #;(define primitive/show 'TODO)
  
  #;(define variable/strict 'TODO))