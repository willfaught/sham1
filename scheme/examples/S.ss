(module S mzscheme
  (provide (all-defined))
  
  (define boolean1 #t)
  
  (define boolean2 #f)
  
  (define character #\a)
  
  (define function1 (lambda (x) x))
  
  (define function2 (lambda (x) (lambda (y) x)))
  
  (define integer 1)
  
  (define string "test")
  
  (define tuple (vector-immutable 1 2))
  
  (define unit (vector-immutable)))