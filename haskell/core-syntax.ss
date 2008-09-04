(module core-syntax mzscheme
  (provide (all-defined))
  
  (define-struct core-syntax () #f)
  
  (define-struct (application core-syntax) (operator operand) #f)
  
  (define-struct (character core-syntax) (value) #f)
  
  (define-struct (constructor core-syntax) (name fields) #f)
  
  (define-struct (data core-syntax) (name constructors) #f)
  
  (define-struct (field core-syntax) (name type) #f)
  
  (define-struct (float core-syntax) (value) #f)
  
  (define-struct (function core-syntax) (parameter body) #f)
  
  (define-struct (haskell core-syntax) (type name) #f)
  
  (define-struct (identifier core-syntax) (name) #f)
  
  (define-struct (if core-syntax) (guard then else) #f)
  
  (define-struct (import core-syntax) (path alias) #f)
  
  (define-struct (integer core-syntax) (value) #f)
  
  (define-struct (let core-syntax) (declarations body) #f)
    
  (define-struct (list-constructor core-syntax) () #f)
  
  (define-struct (ml core-syntax) (type name) #f)
  
  (define-struct (module core-syntax) (name imports declarations) #f)
  
  (define-struct (scheme core-syntax) (type name) #f)
  
  (define-struct (tuple-constructor core-syntax) (arity) #f)
  
  (define-struct (unit-constructor core-syntax) () #f))