(module types mzscheme
  (require (lib "match.ss"))
  
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
  
  ; map-type :: (type -> type) -> type -> type
  (define (map-type mapper type)
    (match type
      (($ function-type t) (mapper (make-function-type (map mapper t))))
      (($ list-type t) (mapper (make-list-type (mapper t))))
      (($ tuple-type t) (mapper (make-tuple-type (map mapper t))))
      (($ universal-type v t) (mapper (make-universal-type v (map-type (lambda (x) (if (member x v) x (mapper x))) t))))
      (t (mapper t))))
  
  ; translate-type-constructors :: type -> type
  (define (translate-type-constructors type)
    (map-type (match-lambda (($ type-constructor "Bool") (make-boolean-type))
                            (($ type-constructor "Char") (make-character-type))
                            (($ type-constructor "Int") (make-integer-type))
                            (($ type-constructor "Integer") (make-integer-type))
                            (($ type-constructor "Float") (make-float-type))
                            (type type)) type))
  
  ; fresh-type-variable :: type-variable
  (define (fresh-type-variable)
    (set! type-variable-count (+ type-variable-count 1))
    (make-type-variable (string-append "t" (number->string type-variable-count)))))
