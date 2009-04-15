(module Types scheme
  (provide (all-defined-out))
  
  (define-struct Type () #:transparent)
  
  (define-struct (Application Type) (operator operand) #:transparent)
  
  (define-struct (Constructor Type) (name) #:transparent)
  
  (define-struct (Function Type) () #:transparent)
  
  (define-struct (List Type) () #:transparent)
  
  (define-struct (Tuple Type) (arity) #:transparent)
  
  (define-struct (Unit Type) () #:transparent)
  
  (define-struct (Variable Type) (name) #:transparent)
  
  (define variables
    (match-lambda
      ((struct Application (r d)) (append (variables r) (variables d)))
      ((? Variable? x) (list x))
      (_ null))))