(module prelude mzscheme
  (require (only (lib "43.ss" "srfi") vector-map)
           (lib "types.ss" "haskell")
           (lib "match.ss"))
  
  (provide (all-defined))
  
  ;(define (prelude identifier)
  ;  (make-immutable-hash-table `(("+" . (compile-term 
  
  (define (strict term)
    (let ((value (force term)))
      (cond ((or (pair? value) (list? value)) (if (null? value) null (cons (strict (car value)) (strict (cdr value)))))
            ((vector? value) (vector-map (lambda (i x) (strict x)) value))
            (else value))))
  
  (define s strict)
  
  (define prelude-types
    (make-immutable-hash-table `(("error" . ,(let ((t (fresh-type-variable)))
                                             (make-universal-type (list t) (make-function-type (make-list-type (make-character-type)) t)))))
                                 ("trace" . ,(let ((t1 (fresh-type-variable))
                                                   (t2 (fresh-type-variable)))
                                             (make-universal-type (list t1 t2) (make-function-type t1 (make-function-type t2 t2)))))
                                 ("+" . ,(make-function-type (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("-" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("*" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("/" . ,(make-function-type (list (make-integer-type) (make-integer-type) (make-integer-type))))
                                 ("==" . ,(let ((t (fresh-type-variable)))
                                            (make-universal-type (list t) (make-function-type (list t t (make-boolean-type))))))
                                 ("/=" . ,(let ((t (fresh-type-variable)))
                                            (make-function-type (list t t (make-boolean-type)))))
                                 (":" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list t (make-list-type t) (make-list-type t))))))
                                 ("head" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) t)))))
                                 ("tail" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) (make-list-type t))))))
                                 ("null" . ,(let ((t (fresh-type-variable)))
                                         (make-universal-type (list t) (make-function-type (list (make-list-type t) (make-boolean-type))))))
                                 ("fst" . ,(let ((t1 (fresh-type-variable))
                                               (t2 (fresh-type-variable)))
                                         (make-universal-type (list t1 t2) (make-function-type (list (make-tuple-type (list t1 t2)) t1)))))
                                 ("snd" . ,(let ((t1 (fresh-type-variable))
                                               (t2 (fresh-type-variable)))
                                         (make-universal-type (list t1 t2) (make-function-type (list (make-tuple-type (list t1 t2)) t2)))))
                                 ("True" . ,(make-boolean-type))
                                 ("False" . ,(make-boolean-type))
                                 ("&&" . ,(make-function-type (list (make-boolean-type) (make-boolean-type) (make-boolean-type))))
                                 ("||" . ,(make-function-type (list (make-boolean-type) (make-boolean-type) (make-boolean-type))))
                                 ("not" . ,(make-function-type (list (make-boolean-type) (make-boolean-type))))) 'equal))
                                 
  
  (define prelude-declarations '("error" "trace" "+" "-" "*" "/" "==" "/=" ":" "head" "tail" "null" "fst" "snd" "True" "False" "&&" "||" "not"))
  
  (define force-list (match-lambda ((h . t) (cons (force h) (force-list (force t))))
                                   (() null)))
  
  )
