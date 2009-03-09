(module HaskellSyntax scheme
  (provide (all-defined-out))
  
  (define-struct HaskellSyntax () #:transparent)
  
  (define-struct (Application HaskellSyntax) (operator operand) #:transparent)
  
  (define-struct (Character HaskellSyntax) (value) #:transparent)
  
  (define-struct (Constructor HaskellSyntax) (name fields) #:transparent)
  
  (define-struct (Data HaskellSyntax) (name constructors) #:transparent)
  
  (define-struct (Declaration HaskellSyntax) (lhs rhs) #:transparent)
  
  (define-struct (Field HaskellSyntax) (names type) #:transparent)
  
  (define-struct (Float HaskellSyntax) (value) #:transparent)
  
  (define-struct (Function HaskellSyntax) (parameters body) #:transparent)
  
  (define-struct (If HaskellSyntax) (guard then else) #:transparent)
  
  (define-struct (Impdecl HaskellSyntax) (language module imports) #:transparent)
  
  (define-struct (Import HaskellSyntax) (name type) #:transparent)
  
  (define-struct (Integer HaskellSyntax) (value) #:transparent)
  
  (define-struct (Let HaskellSyntax) (declarations body) #:transparent)
  
  (define-struct (LHS HaskellSyntax) (name parameters) #:transparent)
  
  (define-struct (List HaskellSyntax) (elements) #:transparent)
  
  (define-struct (ListConstructor HaskellSyntax) () #:transparent)
  
  (define-struct (Module HaskellSyntax) (name exports impdecls declarations) #:transparent)
  
  (define-struct (Tuple HaskellSyntax) (elements) #:transparent)
  
  (define-struct (TupleConstructor HaskellSyntax) (arity) #:transparent)
  
  (define-struct (UnitConstructor HaskellSyntax) () #:transparent)
  
  (define-struct (Variable HaskellSyntax) (name) #:transparent))
