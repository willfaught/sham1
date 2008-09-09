(module CoreSyntax mzscheme
  (provide (all-defined))
  
  (define-struct CoreSyntax () #f)
  
  (define-struct (Application CoreSyntax) (operator operand) #f)
  
  (define-struct (Character CoreSyntax) (value) #f)
  
  (define-struct (Constructor CoreSyntax) (name fields) #f)
  
  (define-struct (Data CoreSyntax) (name constructors) #f)
  
  (define-struct (Declaration CoreSyntax) (lhs rhs) #f)
  
  (define-struct (Field CoreSyntax) (name type) #f)
  
  (define-struct (Float CoreSyntax) (value) #f)
  
  (define-struct (Function CoreSyntax) (parameter body) #f)
  
  (define-struct (Haskell CoreSyntax) (type name) #f)
  
  (define-struct (If CoreSyntax) (guard then else) #f)
  
  (define-struct (Integer CoreSyntax) (value) #f)
  
  (define-struct (Let CoreSyntax) (declarations body) #f)
    
  (define-struct (ListConstructor CoreSyntax) () #f)
  
  (define-struct (ML CoreSyntax) (type name) #f)
  
  (define-struct (Module CoreSyntax) (name imports declarations) #f)
  
  (define-struct (Scheme CoreSyntax) (type contract name) #f)
  
  (define-struct (TupleConstructor CoreSyntax) (arity) #f)
  
  (define-struct (UnitConstructor CoreSyntax) () #f)
  
  (define-struct (Variable CoreSyntax) (name) #f))