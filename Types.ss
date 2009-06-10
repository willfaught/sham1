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
  
  (define typeConstructors
    (match-lambda
      ((struct Application (r d)) (append (typeConstructors r) (typeConstructors d)))
      ((struct Variable (_)) null)
      (x (list x))))
  
  (define typeVariables
    (match-lambda
      ((struct Application (r d)) (append (typeVariables r) (typeVariables d)))
      ((? Variable? x) (list x))
      (_ null))))