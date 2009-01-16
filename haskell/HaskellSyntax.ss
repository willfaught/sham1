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
  
  (define-struct (FunctionType HaskellSyntax) (parameter result) #:transparent)
  
  (define-struct (If HaskellSyntax) (guard then else) #:transparent)
  
  (define-struct (Impdecl HaskellSyntax) (path spec) #:transparent)
  
  (define-struct (Import HaskellSyntax) (name alias type) #:transparent)
  
  (define-struct (Impspec HaskellSyntax) (hiding imports) #:transparent)
  
  (define-struct (Integer HaskellSyntax) (value) #:transparent)
  
  (define-struct (Let HaskellSyntax) (declarations body) #:transparent)
  
  (define-struct (LHS HaskellSyntax) (name parameters) #:transparent)
  
  (define-struct (List HaskellSyntax) (elements) #:transparent)
  
  (define-struct (ListConstructor HaskellSyntax) () #:transparent)
  
  (define-struct (ListType HaskellSyntax) (type) #:transparent)
  
  (define-struct (ML HaskellSyntax) (type name) #:transparent)
  
  (define-struct (Module HaskellSyntax) (name exports imports declarations) #:transparent)
  
  (define-struct (Scheme HaskellSyntax) (type name) #:transparent)
  
  (define-struct (Tuple HaskellSyntax) (elements) #:transparent)
  
  (define-struct (TupleConstructor HaskellSyntax) (arity) #:transparent)
  
  (define-struct (TupleType HaskellSyntax) (types) #:transparent)
  
  (define-struct (TypeApplication HaskellSyntax) (types) #:transparent)
  
  (define-struct (TypeConstructor HaskellSyntax) (name) #:transparent)
  
  (define-struct (TypeVariable HaskellSyntax) (name) #:transparent)
  
  (define-struct (UnitConstructor HaskellSyntax) () #:transparent)
  
  (define-struct (UnitType HaskellSyntax) () #:transparent)
  
  (define-struct (Variable HaskellSyntax) (name) #:transparent))