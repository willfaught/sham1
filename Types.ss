(module Types scheme
  (define-struct Type () #:transparent)
  (provide/contract (struct Type ()))
  
  (define-struct (Application Type) (operator operand) #:transparent)
  (provide/contract (struct Application ((operator Type?) (operand Type?))))
  
  (define-struct (Constructor Type) (name) #:transparent)
  (provide/contract (struct Constructor ((name string?))))
  
  (define-struct (Function Type) () #:transparent)
  (provide/contract (struct Function ()))
  
  (define-struct (List Type) () #:transparent)
  (provide/contract (struct List ()))
  
  (define-struct (Tuple Type) (arity) #:transparent)
  (provide/contract (struct Tuple ((arity integer?))))
  
  (define-struct (Unit Type) () #:transparent)
  (provide/contract (struct Unit ()))
  
  (define-struct (Variable Type) (name) #:transparent)
  (provide/contract (struct Variable ((name string?)))))