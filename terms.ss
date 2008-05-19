(module terms mzscheme
  (provide (all-defined))
  
  (define-struct term () #f)
  
  ; application-term :: term (term)
  (define-struct (application-term term) (function arguments) #f)
  
  ; case-term :: term ((string term))
  (define-struct (case-term term) (expression alternates) #f)
  
  ; character-term :: char
  (define-struct (character-term term) (character) #f)
  
  ; data-term :: string (constructor-term)
  (define-struct (data-term term) (identifier constructors) #f)
  
  ; constructor-term :: string (data-field-term)
  (define-struct (constructor-term term) (identifier fields) #f)
  
  ; field-term :: string type
  (define-struct (field-term term) (identifier type) #f)
  
  ; declaration-term :: (string) term
  (define-struct (declaration-term term) (patterns expression) #f)
  
  ; float-term :: number
  (define-struct (float-term term) (float) #f)
  
  ; function-term :: (string) term
  (define-struct (function-term term) (patterns body) #f)
  
  ; haskell-term :: type term
  (define-struct (haskell-term term) (type term) #f)
  
  ; identifier-term :: string
  (define-struct (identifier-term term) (identifier) #f)
  
  ; if-term :: term term term
  (define-struct (if-term term) (guard then else) #f)
  
  ; import-term :: string string
  (define-struct (import-term term) (path alias) #f)
  
  ; integer-term :: integer
  (define-struct (integer-term term) (integer) #f)
  
  ; let-term :: (declaration-term) term
  (define-struct (let-term term) (declarations expression) #f)
  
  ; list-term :: (term)
  (define-struct (list-term term) (expressions) #f)
  
  ; ml-term :: type string
  (define-struct (ml-term term) (type identifier) #f)
  
  ; module-term :: string import-term (declaration-term)
  (define-struct (module-term term) (identifier imports declarations) #f)
  
  ; scheme-term :: type string
  (define-struct (scheme-term term) (type identifier) #f)
  
  ; tuple-term :: (term)
  (define-struct (tuple-term term) (expressions) #f)
  
  ; tuplecon-term :: integer
  (define-struct (tuplecon-term term) (arity) #f))