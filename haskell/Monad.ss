(module Monad mzscheme
  (require (lib "match.ss")
           (lib "Maybe.ss" "sham" "haskell"))
  
  (provide (all-defined))
  
  (define (bind x y)
    (match (list x y)
      (((? Maybe? _) (? procedure? _)) (maybeBind x y))))
  
  (define (sequence x y)
    (match (list x y)
      (((? Maybe? _) (? Maybe? _)) (maybeSequence x y))))
  
  (define-syntax do
    (syntax-rules ()
      ((do (n <- e) rest) (bind e (lambda (n) rest)))
      ((do e) e)
      ((do e1 e2) (sequence e1 e2))
      ((do e1 e2 ...) (do e1 (do e2 ...))))))