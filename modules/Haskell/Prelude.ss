(module Prelude scheme
  (require (lib "List.ss" "sham" "haskell")
           (lib "Primitives.ss" "sham" "haskell"))
  
  (provide (rename-out (type/Bool/haskell Bool/haskell)
                       (type/Char/haskell Char/haskell)
                       (type/Float/haskell Float/haskell)
                       (type/Int/haskell Int/haskell)
                       (type/List#/haskell List#/haskell)
                       (type/Tuple#/haskell Tuple#/haskell)
                       (type/Unit#/haskell Unit#/haskell)
                       (variable/False False)
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
                       (variable/intAdd intAdd)
                       (variable/intAdd +)
                       (variable/intSubtract intSubtract)
                       (variable/intSubtract -)
                       (variable/intMultiply intMultiply)
                       (variable/intMultiply *)
                       (variable/intDivide intDivide)
                       (variable/intDivide /)
                       (variable/intEqual intEqual)
                       (variable/intEqual ==)
                       (variable//= /=)
                       (variable/floatAdd floatAdd)
                       (variable/floatSubtract floatSubtract)
                       (variable/floatMultiply floatMultiply)
                       (variable/floatDivide floatDivide)
                       (variable/charEqual charEqual)
                       (variable/isLower isLower)
                       (variable/isUpper isUpper)
                       (variable/toLower toLower)
                       (variable/toUpper toUpper)
                       (variable/: :)))
  
  (define type/Bool/haskell
    (promise/c (recursive-contract (or/c (constructor/False/c) (constructor/True/c)))))
  
  (define type/Char/haskell
    (struct/c constructor/Char# char?))
  
  (define type/Float/haskell
    (struct/c constructor/Float# number?))
  
  (define type/Int/haskell
    (struct/c constructor/Int# integer?))
  
  (define type/List#/haskell
    (lambda (typeVariable/a)
      (promise/c (recursive-contract (or/c (constructor/Nil#/c) (constructor/Cons#/c (promise/c typeVariable/a) (promise/c (type/List#/haskell typeVariable/a))))))))
  
  (define (type/Tuple#/haskell arity)
    (let ((vars (map (lambda (x) (string->symbol (string-append "x" (number->string x)))) (iterate (lambda (x) (+ x 1)) 1 arity))))
      (eval `(curry (lambda ,vars
                      (promise/c (list/c ,@vars)))))))
  
  (define type/Unit#/haskell
    (promise/c (recursive-contract (or/c (constructor/Unit#/c)))))
  
  (define variable/False
    (delay (make-constructor/False)))
  
  (define variable/Nil#
    (delay (make-constructor/Nil#)))
  
  (define variable/True
    (delay (make-constructor/True)))
  
  (define variable/Unit#
    (delay (make-constructor/Unit#)))
  
  (define variable/charEqual
    (delay (lambda (x)
             (lambda (y)
               (if (equal? (constructor/Char#-value (force x)) (constructor/Char#-value (force y)))
                   (force variable/True)
                   (force variable/False))))))
  
  (define variable/error
    (delay (lambda (x) (error (string-append "*** Exception: " "TODO")))))
  
  (define variable/floatAdd
    (delay (lambda (x)
             (lambda (y)
               (+ (constructor/Float#-value (force x)) (constructor/Float#-value (force y)))))))
  
  (define variable/floatDivide
    (delay (lambda (x)
             (lambda (y)
               (/ (constructor/Float#-value (force x)) (constructor/Float#-value (force y)))))))
  
  (define variable/floatEqual
    (delay (lambda (x)
             (lambda (y)
               (if (= (constructor/Float#-value (force x)) (constructor/Float#-value (force y)))
                   (force variable/True)
                   (force variable/False))))))
  
  (define variable/floatMultiply
    (delay (lambda (x)
             (lambda (y)
               (* (constructor/Float#-value (force x)) (constructor/Float#-value (force y)))))))
  
  (define variable/floatSubtract
    (delay (lambda (x)
             (lambda (y)
               (- (constructor/Float#-value (force x)) (constructor/Float#-value (force y)))))))
  
  (define variable/fst
    (delay (lambda (t) (force (list-ref (force t) 0)))))
  
  (define variable/head
    (delay (lambda (x) (force (constructor/Cons#-head (force x))))))
  
  (define variable/intAdd
    (delay (lambda (x)
             (lambda (y)
               (+ (constructor/Int#-value (force x)) (constructor/Int#-value (force y)))))))
  
  (define variable/intDivide
    (delay (lambda (x)
             (lambda (y)
               (quotient (constructor/Int#-value (force x)) (constructor/Int#-value (force y)))))))
  
  (define variable/intEqual
    (delay (lambda (x)
             (lambda (y)
               (if (= (constructor/Int#-value (force x)) (constructor/Int#-value (force y)))
                   (force variable/True)
                   (force variable/False))))))
  
  (define variable/intMultiply
    (delay (lambda (x)
             (lambda (y)
               (* (constructor/Int#-value (force x)) (constructor/Int#-value (force y)))))))
  
  (define variable/intSubtract
    (delay (lambda (x)
             (lambda (y)
               (- (constructor/Int#-value (force x)) (constructor/Int#-value (force y)))))))
  
  (define variable/isFalse
    (delay (lambda (x) (if (constructor/False? (force x))
                           (force variable/True)
                           (force variable/False)))))
  
  (define variable/isLower
    (delay (lambda (x)
             (char-lower-case? (constructor/Char#-value (force x))))))
  
  (define variable/isUpper
    (delay (lambda (x)
             (char-upper-case? (constructor/Char#-value (force x))))))
  
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
  
  (define variable/toLower
    (delay (lambda (x)
             (char-downcase (constructor/Char#-value (force x))))))
  
  (define variable/toUpper
    (delay (lambda (x)
             (char-upcase (constructor/Char#-value (force x))))))
  
  (define variable/:
    (delay (lambda (x) (lambda (y) (make-constructor/Cons# x y)))))
  
  (define variable//=
    (delay (lambda (x)
             (lambda (y)
               (if (not (= (constructor/Int#-value (force x)) (constructor/Int#-value (force y))))
                   (force variable/True)
                   (force variable/False)))))))