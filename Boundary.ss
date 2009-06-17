(module Boundary scheme
  (require (lib "Types.ss" "sham"))
  
  (provide boundaryHH contractH)
  
  (define tyvarBindings
    (match-lambda ((struct Variable (n)) `((list ,(string->symbol (string-append "typeVariable/wrap/" n))
                                                 ,(string->symbol (string-append "typeVariable/unwrap/" n))
                                                 ,(string->symbol (string-append "typeVariable/" n)))
                                           (coffer)))))
  
  (define (boundaryHH type)
    (let* ((bindings (map tyvarBindings (remove-duplicates (typeVariables type))))
           (openContract (contractH type))
           (closedContract (if (null? bindings) openContract `(match-let ,openContract ,(contractH type)))))
      `(lambda (x) (contract ,closedContract x 'ThatHaskell 'ThisHaskell))))
  
  (define contractH
    (match-lambda
      ((struct Application ((struct Application ((struct Function ()) p)) r))
       (let ((tyvarContract (lambda (x) (string->symbol (string-append "typeVariable/" x)))))
         (match (list p r)
           ((list (struct Variable (p)) (struct Variable (r))) `(-> ,(tyvarContract p) ,(tyvarContract r)))
           ((list (struct Variable (p)) r) `(-> ,(tyvarContract p) ,(contractH r)))
           ((list p (struct Variable (r))) `(-> ,(contractH p) ,(tyvarContract r)))
           ((list p r) `(-> ,(contractH p) ,(contractH r))))))
      ((struct Application (r d)) `(,(contractH r) ,(contractH d)))
      ((struct Constructor (n)) (string->symbol (string-append "type/" n "/haskell")))
      ((struct List ()) 'type/Haskell.Prelude.List#/haskell)
      ((struct Tuple (a)) `(type/Haskell.Prelude.Tuple#/haskell ,a))
      ((struct Unit ()) 'type/Haskell.Prelude.Unit#/haskell)
      ((struct Variable (n)) (string->symbol (string-append "typeVariable/" n)))))
  
  (define convertHS 'TODO))