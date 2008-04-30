(module types mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons)
           (lib "match.ss"))
  
  (provide (all-defined-except type-variable-count))
  
  (define-struct type () #f)
  (define-struct (boolean-type type) () #f)
  (define-struct (character-type type) () #f)
  (define-struct (float-type type) () #f)
  (define-struct (function-type type) (parameter-type result-type) #f)
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
      (($ function-type p r) (mapper (make-function-type (map-type mapper p) (map-type mapper r))))
      (($ list-type t) (mapper (make-list-type (map-type mapper t))))
      (($ tuple-type t) (mapper (make-tuple-type (map (lambda (x) (map-type mapper x)) t))))
      (($ universal-type v t) (mapper (make-universal-type v (map-type (lambda (x) (if (member x v) x (mapper x))) t))))
      (t (mapper t))))
  
  ; translate-type-constructor :: type-constructor -> type
  (define (translate-type-constructor type)
    (match type
      (($ type-constructor "Bool") (make-boolean-type))
      (($ type-constructor "Char") (make-character-type))
      (($ type-constructor "Int") (make-integer-type))
      (($ type-constructor "Integer") (make-integer-type))
      (($ type-constructor "Float") (make-float-type))
      (type type)))
  
  ; fresh-type-variable :: type-variable
  (define (fresh-type-variable)
    (set! type-variable-count (+ type-variable-count 1))
    (make-type-variable (string-append "t" (number->string type-variable-count))))
  
  ; normalize-type-variables :: type -> type
  (define (normalize-type-variables type)
    (define type-variable-count 0)
    (define mappings null)
    (define (next-type-variable)
      (set! type-variable-count (+ type-variable-count 1))
      (make-type-variable (if (equal? type-variable-count 1) "t" (string-append "t" (number->string (- type-variable-count 1))))))
    (define (rename-type-variable type-variable)
      (match (assoc type-variable mappings)
        ((_ . t) t)
        (#f (let ((t (next-type-variable)))
              (set! mappings (alist-cons type-variable t mappings))
              t))))
    (define (normalize-type-variables type)
      (match type
        (($ function-type p r) (make-function-type (normalize-type-variables p) (normalize-type-variables r)))
        (($ list-type t) (make-list-type (normalize-type-variables t)))
        (($ tuple-type t) (make-tuple-type (map normalize-type-variables t)))
        (($ type-variable i) (rename-type-variable (make-type-variable i)))
        (type type)))
    (normalize-type-variables type)))