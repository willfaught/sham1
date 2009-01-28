(module Maybe mzscheme
  (require (lib "contract.ss")
           (lib "match.ss"))
  
  (provide (struct Maybe ())
           (struct Nothing ())
           (struct Just (value)))
  
  (define-struct Maybe () #f)
  
  (define-struct (Nothing Maybe) () #f)
  
  (define-struct (Just Maybe) (value) #f))