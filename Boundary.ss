(module Boundary scheme
  (require (lib "List.ss" "sham" "haskell")
           (lib "Types.ss" "sham"))
  
  (provide boundaryHH contractH)
  
  (define tyvarBindings
    (match-lambda ((struct Variable (n)) `((list ,(string->symbol (string-append "typeVariable/" n))
                                                 ,(string->symbol (string-append "typeVariable/" n "/wrap"))
                                                 ,(string->symbol (string-append "typeVariable/" n "/unwrap")))
                                           (coffer)))))
  
  (define (boundaryHH type syntax thisName thatName)
    (let* ((bindings (map tyvarBindings (remove-duplicates (typeVariables type))))
           (openContract `(promise/c ,(contractH type)))
           (closedContract (if (null? bindings) openContract `(match-let ,bindings ,openContract))))
      `(contract ,closedContract ,syntax ',(string->symbol thatName) ',(string->symbol thisName))))
  
  (define (splitPath path)
    (regexp-split #rx"\\." path))
  
  (define (typeName type)
    (last (splitPath type)))
  
  (define (typeModule language type)
    (let ((path (splitPath type)))
      (if (= (length path) 1) 
          (match language
            ("haskell" "Haskell.Prelude")
            #;("ml" 'TODO)
            #;("scheme" 'TODO))
          (foldl1 (lambda (x y) (string-append y "." x)) path))))
  
  (define contractH
    (match-lambda
      ((struct Application ((struct Application ((struct Function ()) p)) r))
       (let ((tyvarContract (lambda (x) (string->symbol (string-append "typeVariable/" x)))))
         (match (list p r)
           ((list (struct Variable (p)) (struct Variable (r))) `(-> (promise/c ,(tyvarContract p)) ,(tyvarContract r)))
           ((list (struct Variable (p)) r) `(-> (promise/c ,(tyvarContract p)) ,(contractH r)))
           ((list p (struct Variable (r))) `(-> (promise/c ,(contractH p)) ,(tyvarContract r)))
           ((list p r) `(-> (promise/c ,(contractH p)) ,(contractH r))))))
      ((struct Application (r d)) `(,(contractH r) ,(contractH d)))
      ((struct Constructor (n)) (string->symbol (string-append "type/" (typeModule "haskell" n) "." (typeName n) "/haskell")))
      ((struct List ()) 'type/Haskell.Prelude.List/haskell)
      ((struct Tuple (a)) (let ((vars (map (lambda (x) (string->symbol (string-append "x" (number->string x))))
                                           (iterate (lambda (x) (+ x 1)) 1 a))))
                            `(curry (lambda ,vars (constructor/Tuple#/c (list/c ,@(map (lambda (x) `(promise/c ,x)) vars)))))))
      ((struct Unit ()) 'type/Haskell.Prelude.Unit/haskell)
      ((struct Variable (n)) (string->symbol (string-append "typeVariable/" n)))))
  
  (define convertHS 'TODO))