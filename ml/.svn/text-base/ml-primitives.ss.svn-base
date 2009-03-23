(module ml-primitives mzscheme
  (provide (all-defined))
  
  (define (new-wrapper)
    (define-struct wrapper (val))
    (values make-wrapper wrapper-val wrapper?))  
  
  ; Used for importing and exporting tuples
  (define ((tuple-predicate member-predicates) arg)
    (and (vector? arg)
         (= (vector-length arg)
            (length member-predicates))
         (andmap (lambda (f x) (f x)) member-predicates (vector->list arg))))
  
  (define (any? x) #t)
  
  ; This function currently will only be called with ints. It can't be given a polymorphic type because then it won't work.
  ; Since the ml typechecker will run through this, we can assume that x and y are of the same type.
  ; However, we don't know what that type is. If it is a function type, raise an exception, like ocaml.
  (define ml-equals
    (lambda (x)
      (lambda (y)
        (cond 
          [(procedure? x) (raise "Invalid_argument \"equal: functional value\"")]
          [(integer? x) (= x y)]
          [else (equal? x y)]))))
  
  (define ml-ne
    (lambda (x)
      (lambda (y)
        (not ((ml-equals x) y)))))  
  
  (define ml-ge
    (lambda (x)
      (lambda (y)
        (>= x y))))
  
  (define ml-gt
    (lambda (x)
      (lambda (y)
        (> x y))))
  
  (define ml-le
    (lambda (x)
      (lambda (y)
        (<= x y))))  

  (define ml-lt
    (lambda (x)
      (lambda (y)
        (< x y))))  

  (define ml-times
    (lambda (x)
      (lambda (y)
        (* x y))))  
  
  (define ml-divide
    (lambda (x)
      (lambda (y)
        (floor (/ x y)))))

  (define ml-modulo
    (lambda (n)
      (lambda (b)
        (modulo n b))))  
  
  (define ml-plus
    (lambda (x)
      (lambda (y)
        (+ x y))))  

  (define ml-minus
    (lambda (x)
      (lambda (y)
        (- x y))))  
  )
