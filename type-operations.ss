(module type-operations mzscheme
  (require (only (lib "1.ss" "srfi") alist-cons)
           (lib "match.ss")
           (lib "types.ss" "sham"))
  
  (provide (all-defined-except type-variable-count))

  (define type-variable-count 0)
  
  ; new-type-variable :: type-variable
  (define (new-type-variable)
    (set! type-variable-count (+ type-variable-count 1))
    (make-type-variable (string-append "t" (number->string type-variable-count))))
  
  ; normalize-type-variables :: type -> type
  (define (normalize-type-variables type)
    (define type-variable-count 0)
    (define mappings null)
    (define (next-type-variable)
      (set! type-variable-count (+ type-variable-count 1))
      (new-type-variable (if (equal? type-variable-count 1) "t" (string-append "t" (number->string (- type-variable-count 1))))))
    (define (rename-type-variable type-variable)
      (match (assoc type-variable mappings)
        ((_ . t) t)
        (#f (let ((t (next-type-variable))) (set! mappings (alist-cons type-variable t mappings)) t))))
    (define (normalize-type-variables type)
      (match type
        (($ type-application r d) (make-type-application (normalize-type-variables r) (normalize-type-variables d)))
        ((? type-variable? t) (rename-type-variable t))
        (type type)))
    (normalize-type-variables type)))