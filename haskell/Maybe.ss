(module Maybe mzscheme
  (require (lib "contract.ss")
           (lib "match.ss"))
  
  (provide (struct Maybe ())
           (struct Nothing ())
           (struct Just (value)))
  
  (define-struct Maybe () #f)
  
  (define-struct (Nothing Maybe) () #f)
  
  (define-struct (Just Maybe) (value) #f)
  
  (provide/contract (maybeBind (-> Maybe? (-> any/c Maybe?) Maybe?))
                    (maybeReturn (-> any/c Maybe?))
                    (maybeSequence (-> Maybe? Maybe? Maybe?)))
  
  (define (maybeBind x y)
    (match (list x y)
      (((? Nothing? z) _) z)
      ((($ Just z) _) (y z))))
  
  (define (maybeReturn x)
    (make-Just x))
  
  (define (maybeSequence x y)
    (match (list x y)
      (((? Nothing? z) _) z)
      ((($ Just _) (? Nothing? z)) z)
      ((($ Just _) (? Just? z)) z))))