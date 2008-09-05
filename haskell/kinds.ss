(module kinds mzscheme
  (provide (all-defined))
  
  (define-struct kind () #f)
  
  (define-struct (nullary kind) () #f)
  
  (define-struct (binary kind) () #f))