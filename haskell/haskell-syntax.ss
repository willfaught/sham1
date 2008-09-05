(module haskell-syntax mzscheme
  (provide (all-defined))
  
  (define-struct HaskellSyntax () #f)
  
  (define-struct (Application HaskellSyntax) (operator operand) #f)
  
  (define-struct (Body HaskellSyntax) (imports declarations) #f)
  
  (define-struct (Character HaskellSyntax) (value) #f)

  (define-struct (Constructor HaskellSyntax) (name fields) #f)
  
  (define-struct (Data HaskellSyntax) (name constructors) #f)
  
  (define-struct (Declaration HaskellSyntax) (lhs rhs) #f)
  
  (define-struct (Field HaskellSyntax) (name type) #f)
  
  (define-struct (Float HaskellSyntax) (value) #f)
  
  (define-struct (Function HaskellSyntax) (parameters body) #f)
  
  (define-struct (FunctionType HaskellSyntax) (argument result) #f)
  
  (define-struct (If HaskellSyntax) (guard then else) #f)
  
  (define-struct (Integer HaskellSyntax) (value) #f)
  
  (define-struct (Let HaskellSyntax) (declarations body) #f)
  
  (define-struct (LHS HaskellSyntax) (name parameters) #f)
  
  (define-struct (List HaskellSyntax) (elements) #f)
  
  (define-struct (ListConstructor HaskellSyntax) () #f)
  
  (define-struct (ListType HaskellSyntax) (type) #f)
  
  (define-struct (ML HaskellSyntax) (type name) #f)
  
  (define-struct (Module HaskellSyntax) (name body) #f)
  
  (define-struct (Scheme HaskellSyntax) (type name) #f)
  
  (define-struct (Tuple HaskellSyntax) (elements) #f)
  
  (define-struct (TupleConstructor HaskellSyntax) (arity) #f)
  
  (define-struct (TupleType HaskellSyntax) (types) #f)
  
  (define-struct (TypeApplication HaskellSyntax) (types) #f)
  
  (define-struct (TypeConstructor HaskellSyntax) (name) #f)
  
  (define-struct (TypeVariable HaskellSyntax) (name) #f)
  
  (define-struct (UnitConstructor HaskellSyntax) () #f)
  
  (define-struct (UnitType HaskellSyntax) () #f)
  
  (define-struct (Variable HaskellSyntax) (name) #f))