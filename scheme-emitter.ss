; Scheme emitter for a Haskell abstract syntax tree.

(module scheme-emitter mzscheme
  (require (lib "match.ss"))
  (provide (all-defined))

  ; Haskell abstract syntax.
  (define-struct hnum (value))
  (define-struct hchar (value))
  (define-struct hlist (head tail))
  (define-struct htup (values))
  (define-struct htype (name cons))
  ;(define-struct (hcon htype
  (define-struct hid (name))
  (define-struct hlet (bindings body))
  (define-struct hcase (exp pats))
  (define-struct hfun (param body))
  (define-struct happ (fun arg))

  ; Emits Scheme code as data.
  (define emit-scheme (match-lambda (($ hnum v) `,v)
                                    (($ hchar v) `,v)
                                    (($ hbool v) `,v)
                                    (($ hid n) (emit-hid n))
                                    (($ hfun p b) `(lambda (,(string->symbol p)) ,(emit-scheme b)))
                                    (($ happ f a) `(,(emit-scheme f) (delay ,(emit-scheme a))))
                                    ;(($ hcons h t) `(list (delay ,(emit-scheme h)) (delay ,(emit-scheme t))))
                                    ))
  
  (define (emit-hid id) (cond ((equal? id "+") '+)
                              ((equal? id "-") '-)
                              ((equal? id "*") '*)
                              ((equal? id "/") '/)
                              (else `(force ,(string->symbol id)))))
  
  ;(define (emit-scheme-list c)
  ;  (define (unnest c) (if (eqv? c 'nil) () `(delay ,(emit-scheme (hcons-head c)))
  
  (define (run-test hexp result) (eqv? (eval (emit-scheme hexp)) result))
  
  (define (run-tests tests) (map (lambda (test) (run-test (car test) (cdr test))) tests))
  
  (define tests (list (cons (make-hnum 1) 1)
                      (cons (make-hadd (make-hnum 1) (make-hnum 2)) 3)
                      (cons (make-hsub (make-hnum 4) (make-hnum 3)) 1)
                      (cons (make-hadd (make-hadd (make-hnum 1) (make-hnum 2)) (make-hsub (make-hnum 4) (make-hnum 3))) 4)
                      (cons (make-hif (make-hadd (make-hnum 1) (make-hnum 2)) (make-hnum 3) (make-hnum 4)) 4)
                      (cons (make-hif (make-hnum 0) (make-hadd (make-hnum 2) (make-hnum 3)) (make-hnum 4)) 5)
                      (cons (make-hif (make-hnum 1) (make-hnum 2) (make-hadd (make-hnum 1) (make-hnum 2))) 3)
                      (cons (make-happ (make-hfun "x" (make-hid "x")) (make-hnum 1)) 1)
                      (cons (make-happ (make-hfun "x" (make-hadd (make-hid "x") (make-hid "x"))) (make-hadd (make-hnum 1) (make-hnum 1))) 4)))
)