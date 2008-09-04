(module haskell-syntax mzscheme
  (provide (all-defined))
  
  (define-struct haskell-syntax () #f)
  
  (define-struct (application haskell-syntax) (elements) #f)
  
  (define-struct (character haskell-syntax) (value) #f)
  
  (define-struct (constructor haskell-syntax) (name fields) #f)
  
  (define-struct (data haskell-syntax) (name constructors) #f)
  
  (define-struct (declaration haskell-syntax) (name parameters rhs) #f)
  
  (define-struct (field haskell-syntax) (name type) #f)
  
  (define-struct (float haskell-syntax) (value) #f)
  
  (define-struct (function haskell-syntax) (parameters body) #f)
  
  (define-struct (variable haskell-syntax) (name) #f)
  
  (define-struct (if haskell-syntax) (guard then else) #f)
  
  (define-struct (integer haskell-syntax) (value) #f)
  
  (define-struct (let haskell-syntax) (declarations body) #f)
  
  (define-struct (list haskell-syntax) (elements) #f)
  
  (define-struct (module haskell-syntax) (name imports declarations) #f)
  
  (define-struct (tuple haskell-syntax) (elements) #f)
  
  (define-struct (unit haskell-syntax) () #f))