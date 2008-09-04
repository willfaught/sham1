(module terms mzscheme
  (provide (all-defined))
  
  (define-struct term () #f)
  
  (define-struct (application-term term) (operator operand) #f)
  
  (define-struct (character-term term) (character) #f)
  
  (define-struct (data-term term) (name constructors) #f)
  
  (define-struct (constructor-term term) (name fields) #f)
  
  (define-struct (field-term term) (name type) #f)
  
  (define-struct (float-term term) (float) #f)
  
  (define-struct (function-term term) (parameter body) #f)
  
  (define-struct (haskell-term term) (type term) #f)
  
  (define-struct (identifier-term term) (name) #f)
  
  (define-struct (if-term term) (guard then else) #f)
  
  (define-struct (import-term term) (path alias) #f)
  
  (define-struct (integer-term term) (integer) #f)
  
  (define-struct (let-term term) (declarations body) #f)
    
  (define-struct (list-constructor-term) () #f)
  
  (define-struct (ml-term term) (type identifier) #f)
  
  (define-struct (module-term term) (identifier imports declarations) #f)
  
  (define-struct (scheme-term term) (type identifier) #f)
  
  (define-struct (tuple-constructor-term term) (arity) #f)
  
  (define-struct (unit-constructor-term term) () #f))