(module Compilation scheme
  (require (lib "Boundary.ss" "sham")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide compileModule compileSyntax)
  
  (define constructorDefinition
    (match-lambda
      ((struct c/Constructor (n _))
       #`(define #,(toSyntax "haskell/" n)
           (delay (curry #,(toSyntax "make-haskell/constructor/" n)))))))
  
  (define constructorPredicate
    (match-lambda
      ((struct c/Constructor (n _))
       #`(define #,(toSyntax "haskell/is" n)
           (delay (lambda (x)
                    (if (#,(toSyntax "haskell/constructor/" n "?") (force x))
                        (force haskell/True)
                        (force haskell/False))))))))
  
  (define (constructorType typeName constructor)
    (match-let (((struct c/Constructor (n f)) constructor))
      (let ((cs (toSyntax "haskell/constructor/" n))
            (ts (toSyntax "haskell/type/" typeName))
            (fs (map (lambda (x) (toSyntax x)) (map (match-lambda ((struct c/Field (n _)) n)) f))))
        #`(define-struct (#,cs #,ts) #,fs #:transparent))))
  
  (define (constructor typeName syntax)
    (let ((type (constructorType typeName syntax))
          (definition (constructorDefinition syntax))
          (predicate (constructorPredicate syntax)))
      (append (list type definition predicate)
              (map (match-lambda ((struct c/Field (n _)) (field (c/Constructor-name syntax) n)))
                   (c/Constructor-fields syntax)))))
  
  (define data
    (match-lambda ((struct c/Data (n c)) (cons (dataDefinition n) (map (curry constructor n) c)))))
  
  (define (dataDefinition typeName)
    #`(define-struct #,(toSyntax "haskell/type/" typeName) () #:transparent))
  
  (define (field constructorName fieldName)
    #`(define #,(toSyntax "haskell/" fieldName)
        (delay (lambda (x) (force (#,(toSyntax "haskell/constructor/" constructorName "-" fieldName) (force x)))))))
  
  (define typeNames
    (match-lambda
      ((struct c/Data (_ c)) (foldl append null (map typeNames c)))
      ((struct c/Constructor (n f)) (cons n (foldl append null (map typeNames f))))
      ((struct c/Field (n _)) (list n))))
  
  (define compileModule
    (match-lambda
      ((struct c/Module (n i d))
       (match-let ((types (filter c/Data? d))
                   (decls (filter c/Declaration? d)))
         #`(module #,(toSyntax n) scheme
             (require (lib "Primitives.ss" "sham" "haskell"))
             #,@(map importRequire i)
             #,@(map (match-lambda ((list x y) (export x y)))
                     (append (foldl append null (map typeExports types)) (map declarationExport decls)))
             #,@(map importDefinition i)
             #,@(foldl append null (map data types))
             #,@(map moduleDeclaration decls))))))
  
  (define compileSyntax
    (match-lambda 
      ((struct c/Application (r d)) #`(#,(compileSyntax r) (delay #,(compileSyntax d))))
      ((struct c/Character (v)) (string-ref v 0))
      ((struct c/Float (v)) (string->number v))
      ((struct c/Function (p b)) #`(lambda (#,(toSyntax "haskell/" p)) #,(compileSyntax b)))
      ((struct c/If (g t e)) #`(if (equal? #,(compileSyntax g) (force haskell/True)) #,(compileSyntax t) #,(compileSyntax e)))
      ((struct c/Integer (v)) (string->number v))
      ((struct c/Let (d b)) #`(letrec #,(map letDeclaration d) #,(compileSyntax b)))
      ((struct c/ListConstructor ()) #'(make-haskell/constructor/#Nil))
      ((struct c/TupleConstructor (a)) (tupleConstructor a))
      ((struct c/UnitConstructor ()) #`(make-haskell/constructor/#Unit))
      ((struct c/Variable (n)) #`(force #,(toSyntax "haskell/" n)))))
  
  (define declarationExport
    (match-lambda
      ((struct c/Declaration (l _)) (list (string-append "haskell/" l) l))))
  
  (define (export internalName externalName)
    #`(provide (rename-out (#,internalName #,externalName))))
  
  (define importDefinition
    (match-lambda
      ((struct c/Import (l _ n a t))
       (let ((name (toSyntax "import/" n)))
         #`(define #,(toSyntax "haskell/" a)
             ,(match l
                ("ml" (boundaryHM t ,name))
                ("scheme" (boundaryHS t ,name))))))))
  
  (define importRequire
    (match-lambda
      ((struct c/Import (_ p n a _))
       #`(require (rename-in (file #,p) (#,n #,(toSyntax "import/" n)))))))
  
  (define letDeclaration
    (match-lambda
      ((struct c/Declaration (l r))
       #`(#,(toSyntax "haskell/" l) (delay #,(compileSyntax r))))))
  
  (define moduleDeclaration
    (match-lambda
      ((struct c/Declaration (l r)) #`(define #,(toSyntax "haskell/" l) (delay ,(compileSyntax r))))))
  
  (define toSyntax
    (lambda s (syntax (string->symbol (foldl (lambda (x y) (string-append y x)) "" s)))))
  
  (define (tupleConstructor arity)
    (let ((vars (map (lambda (x) (toSyntax "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 arity))))
      #`(lambda #,vars (list #,vars))))
  
  (define typeExports
    (match-lambda
      ((struct c/Data (n c))
       (cons (list (string-append "haskell/type/" n "?")
                   (string-append n "?"))
             (map (match-lambda ((struct c/Constructor (n f))
                                 (cons (list (string-append "haskell/" n) n)
                                       (map (match-lambda ((struct c/Field (n _))
                                                           (list (string-append "haskell/" n) n))) f)))) c)))))
  
  (define (typePredicateExport typeName)
    #`(provide (rename-out (#,(toSyntax "haskell/type/" typeName "?") #,(toSyntax typeName "?"))))))