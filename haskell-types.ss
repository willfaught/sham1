(module haskell-types mzscheme
  (provide (all-defined-except type-variable-count))
  
  (define-struct type () #f)
  (define-struct (boolean-type type) () #f)
  (define-struct (character-type type) () #f)
  (define-struct (float-type type) () #f)
  (define-struct (function-type type) (types) #f)
  (define-struct (integer-type type) () #f)
  (define-struct (list-type type) (type) #f)
  (define-struct (tuple-type type) (types) #f)
  (define-struct (type-constructor type) (identifier) #f)
  (define-struct (type-variable type) (identifier) #f)
  (define-struct (universal-type type) (type-variables type) #f)
  
  (define type-variable-count 0)
  
  ; fresh-type-variable :: type-variable
  (define (fresh-type-variable)
    (set! type-variable-count (+ type-variable-count 1))
    (make-type-variable (string-append "t" (number->string type-variable-count))))
  
  (define (debug-reset-type-variable-count)
    (set! type-variable-count 0)))

; algebraic-type?