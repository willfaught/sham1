(module Haskell scheme
  (require (lib "CoreSyntax.ss" "sham" "haskell")
           (lib "Parsers.ss" "sham" "haskell")
           (lib "Types.ss" "sham"))
  
  (provide (rename-out (variable/False False)
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
  
  (define preludeParsers (parsers "Prelude"))
  
  (define declarations#
    (let ((parseT (parser 'type preludeParsers)))
      (map (match-lambda ((list _ t) (parseT t))) (list (list "False" "Bool")
                                                        (list "[]" "[a]")
                                                        (list "True" "Bool")
                                                        (list "()" "()")
                                                        (list "error" "[Char] -> a")
                                                        (list "fst" "(a, b) -> a")
                                                        (list "head" "[a] -> a")
                                                        (list "isFalse" "Bool -> Bool")
                                                        (list "isTrue" "Bool -> Bool")
                                                        (list "null" "[a] -> Bool")
                                                        (list "snd" "(a, b) -> b")
                                                        (list "tail" "[a] -> [a]")
                                                        (list ":" "a -> [a] -> [a]")))))
  
  (define types#
    (map (lambda (x) (parseD x)) (list "data Bool = False | True"
    
  
  (define-struct Char# (value) #:transparent)
  
  (define-struct Float# (value) #:transparent)
  
  (define-struct Int# (value) #:transparent)
  
  (define-struct Tuple# (value) #:transparent)
  
  (define-contract-struct Cons# (head tail) #:transparent)
  
  (define-contract-struct False () #:transparent)
  
  (define-contract-struct Nil# () #:transparent)
  
  (define-contract-struct True () #:transparent)
  
  (define-contract-struct Unit# () #:transparent)
  
  (define Char/haskell/c
    (struct/c Char# char?))
  
  (define Float/haskell/c
    (struct/c Float# number?))
  
  (define Int/haskell/c
    (struct/c Int# integer?))
  
  (define (List#/haskell/c haskell/a/c)
    (recursive-contract (or/c (constructor/Nil#/c) (constructor/Cons#/c (promise/c haskell/a/c) (promise/c (List#? haskell/a/c))))))
  
  (define (Tuple#/haskell/c c)
    (struct/c constructor/Char c))
  
  (define Function#?
    (lambda (contract1)
      (lambda (contract2)
        (-> contract1 contract2))))
  
  (define Bool/haskell/c
    (curry (lambda (language value) (contract (or/c (struct/c constructor/False) (struct/c constructor/True)) value language 'haskell))))
  
  (define (List#? contract1)
    (recursive-contract (or/c (constructor/Nil#/c) (constructor/Cons#/c (promise/c contract1) (promise/c (List#? contract1))))))
  
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