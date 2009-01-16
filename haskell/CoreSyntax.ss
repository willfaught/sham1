(module CoreSyntax scheme
  (provide (all-defined-out))
  
  (define-struct CoreSyntax () #:transparent)
  
  (define-struct (Application CoreSyntax) (operator operand) #:transparent)
  
  (define-struct (Character CoreSyntax) (value) #:transparent)
  
  (define-struct (Constructor CoreSyntax) (name fields) #:transparent)
  
  (define-struct (Data CoreSyntax) (name constructors) #:transparent)
  
  (define-struct (Declaration CoreSyntax) (lhs rhs) #:transparent)
  
  (define-struct (Export CoreSyntax) (name type) #:transparent)
  
  (define-struct (Field CoreSyntax) (name type) #:transparent)
  
  (define-struct (Float CoreSyntax) (value) #:transparent)
  
  (define-struct (Function CoreSyntax) (parameter body) #:transparent)
  
  (define-struct (If CoreSyntax) (guard then else) #:transparent)
  
  (define-struct (Import CoreSyntax) (qualified path alias specs) #:transparent)
  
  (define-struct (Integer CoreSyntax) (value) #:transparent)
  
  (define-struct (Let CoreSyntax) (declarations body) #:transparent)
    
  (define-struct (ListConstructor CoreSyntax) () #:transparent)
  
  (define-struct (ML CoreSyntax) (type name) #:transparent)
  
  (define-struct (Module CoreSyntax) (name exports imports declarations) #:transparent)
  
  (define-struct (Scheme CoreSyntax) (type name) #:transparent)
  
  (define-struct (TupleConstructor CoreSyntax) (arity) #:transparent)
  
  (define-struct (UnitConstructor CoreSyntax) () #:transparent)
  
  (define-struct (Variable CoreSyntax) (name) #:transparent))