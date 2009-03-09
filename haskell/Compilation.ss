(module Compilation scheme
  (require (lib "Boundary.ss" "sham")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (lib "Maybe.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide compileModule compileSyntax)
  
  (define constructorDefinition
    (match-lambda
      ((struct c/Constructor (n _))
       `(define ,(toSymbol "haskell/" n)
          (delay (curry ,(toSymbol "make-haskell/constructor/" n)))))))
  
  (define constructorPredicate
    (match-lambda
      ((struct c/Constructor (n _))
       `(define ,(toSymbol "haskell/is" n)
          (delay (lambda (x)
                   (if (,(toSymbol "haskell/constructor/" n "?") (force x))
                       (force haskell/True)
                       (force haskell/False))))))))
  
  (define (constructorType typeName constructor)
    (match-let (((struct c/Constructor (n f)) constructor))
      (let ((cs (toSymbol "haskell/constructor/" n))
            (ts (toSymbol "haskell/type/" typeName))
            (fs (map (lambda (x) (toSymbol x)) (map (match-lambda ((struct c/Field (n _)) n)) f))))
        `(define-struct (,cs ,ts) ,fs #:transparent))))
  
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
    `(define-struct ,(toSymbol "haskell/type/" typeName) () #:transparent))
  
  (define (field constructorName fieldName)
    `(define ,(toSymbol "haskell/" fieldName)
       (delay (lambda (x) (force (,(toSymbol "haskell/constructor/" constructorName "-" fieldName) (force x)))))))
  
  (define (compileModule syntax types)
    (match syntax
      ((struct c/Module (n e i d))
       (let ((datas (filter c/Data? d))
             (decls (filter c/Declaration? d)))
         `(module ,(toSymbol n) scheme
            ,@(map importRequire i)
            ,@(map importDefinition i)
            ,@(map export e)
            ,@(foldl append null (map data datas))
            ,@(map moduleDeclaration decls))))))
  
  (define compileSyntax
    (match-lambda 
      ((struct c/Application (r d)) `(,(compileSyntax r) (delay ,(compileSyntax d))))
      ((struct c/Character (v)) `,(string-ref v 0))
      ((struct c/Float (v)) `,(string->number v))
      ((struct c/Function (p b)) `(lambda (,(toSymbol "haskell/" p)) ,(compileSyntax b)))
      ((struct c/If (g t e)) `(if (equal? ,(compileSyntax g) (force haskell/True)) ,(compileSyntax t) ,(compileSyntax e)))
      ((struct c/Integer (v)) `,(string->number v))
      ((struct c/Let (d b)) `(letrec ,(map letDeclaration d) ,(compileSyntax b)))
      ((struct c/ListConstructor ()) '(force haskell/Nil#))
      ((struct c/TupleConstructor (a)) (tupleConstructor a))
      ((struct c/UnitConstructor ()) '(force haskell/Unit#))
      ((struct c/Variable (n)) `(force ,(toSymbol "haskell/" n)))))
  
  (define declarationExport
    (match-lambda
      ((struct c/Declaration (l _)) (list (string-append "haskell/" l) l))))
  
  (define export
    (match-lambda
      ((struct c/Export (n)) `(provide (rename-out (,(toSymbol "haskell/" n) ,(toSymbol n)))))))
  
  (define importDefinition
    (match-lambda
      ((struct c/Import (l m n t))
       (let* ((qualifiedName (string-append m "." n))
              (importQualifiedName (toSymbol "import/" qualifiedName)))
         `(define ,(toSymbol "haskell/" qualifiedName)
            ,(match l
               ("haskell" (boundaryHH t importQualifiedName))
               ("ml" (boundaryHM t importQualifiedName))
               ("scheme" (boundaryHS t importQualifiedName))))))))
  
  (define importRequire
    (match-lambda
      ((struct c/Import (l m n _))
       (let ((modules (regexp-split #rx"\\." m)))
         `(require (rename-in (lib ,(string-append (last modules) ".ss") "sham" "modules" ,@(drop-right modules 1))
                              (,(toSymbol n) ,(toSymbol "import/" m "." n))))))))
  
  (define letDeclaration
    (match-lambda
      ((struct c/Declaration (l r))
       `(,(toSymbol "haskell/" l) (delay ,(compileSyntax r))))))
  
  (define moduleDeclaration
    (match-lambda
      ((struct c/Declaration (l r)) `(define ,(toSymbol "haskell/" l) (delay ,(compileSyntax r))))))
  
  (define toSymbol
    (lambda s (string->symbol (foldl (lambda (x y) (string-append y x)) "" s))))
  
  #;(define toSyntax
      (lambda s `,(string->symbol (foldl (lambda (x y) (string-append y x)) "" s))))
  
  (define (tupleConstructor arity)
    (let ((vars (map (lambda (x) (toSymbol "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 arity))))
      `(curry (lambda ,vars (list ,@vars)))))
  
  (define (typePredicateExport typeName)
    `(provide (rename-out (,(toSymbol "haskell/type/" typeName "?") ,(toSymbol typeName "?"))))))