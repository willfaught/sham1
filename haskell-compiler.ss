(module haskell-compiler mzscheme
  (require (lib "match.ss"))
  (provide (all-defined))

  (define-struct tapp (function argument))
  (define-struct tcase (expression patterns))
  (define-struct tchar (value))
  (define-struct temb (expression))
  (define-struct tfun (parameters body))
  (define-struct tid (name))
  (define-struct tlet (bindings body))
  (define-struct tlist (head tail))
  (define-struct tmod (name imports exports types declarations))
  (define-struct tnum (value))
  (define-struct ttype (name constructors))
  
  (define compile-haskell
    (match-lambda (($ tapp f a) `(,(compile-haskell f) (delay ,(compile-haskell a))))
                  ;(($ tcase e p) )
                  (($ tchar v) `,v)
                  ;(($ tcon n t) )
                  ;(($ temb e) )
                  (($ tfun p b) (compile-tfun p b))
                  (($ tid n) (compile-tid n))
                  ;(($ tlet bi bo) )
                  (($ tlist h t) (compile-tlist h t))
                  ;(($ tmod n i e t d) )
                  (($ tnum v) `,v)
                  ;(($ ttype n c) )
                  ))
  
  ; doesn't work yet
  (define (compile-tfun parameters body)
    `(lambda (,(list-ref parameters 0) . rest)
             (define params ',(cdr parameters))
             (define result (wrap params rest \`(let ((,(list-ref parameters 0) 'blah)) (+ w x y z))))
             (if (= (length params) (length rest))
                 result
                 `(lambda (,(list-ref (length rest)))
                    (define params ',(list-tail (length rest)))
                    result))))
  
  (define (compile-tid name)
    (car (hash-table-get prelude name (lambda () `((force ,(string->symbol name)))))))
  
  (define (compile-tlist head tail)
    (cond ((equal? head 'empty-list) '())
          ((equal? tail 'empty-list) `(,(compile-haskell head)))
          (else (list* (compile-haskell head) (compile-tlist (tlist-head tail) (tlist-tail tail))))))
  
  (define (run-test exp result)
    (equal? (eval (compile-haskell exp)) result))
  
  (define (run-tests tests)
    (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  (define (wrap params args exp)
    (if (null? args) exp `(let ((,(car params) ,(car args)) (wrap (cdr params) (cdr args) exp)))))
  
  (define prelude
    (make-immutable-hash-table `(("+" ,+) ; since no type classes yet
                                 ("-" ,-)
                                 ("*" ,*)
                                 ("/" ,/)
                                 (":" ,(lambda (h) (lambda (t) ((make-tlist h t)))))) 'equal))
  
  ;(define tests (list (cons (make-hnum 1) 1)
  ;                    (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)))
)