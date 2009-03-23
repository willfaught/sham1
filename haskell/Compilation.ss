(module Compilation scheme
  (require (lib "Boundary.ss" "sham")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (lib "Maybe.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide compileModule compileSyntax)
  
  (define constructorPredicate
    (match-lambda
      ((struct c/Constructor (n _))
       `(define ,(toSymbol "variable/is" n)
          (delay (lambda (x)
                   (if (,(toSymbol "constructor/" n "?") (force x))
                       (force variable/Haskell.True)
                       (force variable/Haskell.False))))))))
  
  (define (constructorDefinition typeName constructor)
    (match-let (((struct c/Constructor (n f)) constructor))
      (let ((cs (toSymbol "constructor/" n))
            (fs (map (lambda (x) (toSymbol x)) (map (match-lambda ((struct c/Field (n _)) n)) f))))
        `(define-struct ,cs ,fs #:transparent))))
  
  (define (constructor typeName syntax)
    (let ((type (constructorDefinition typeName syntax))
          (definition (curriedConstructor syntax))
          (predicate (constructorPredicate syntax)))
      (append (list type definition predicate)
              (map (match-lambda ((struct c/Field (n _)) (field (c/Constructor-name syntax) n)))
                   (c/Constructor-fields syntax)))))
  
  (define curriedConstructor
    (match-lambda
      ((struct c/Constructor (n _))
       `(define ,(toSymbol "variable/" n)
          (delay (curry ,(toSymbol "make-constructor/" n)))))))
  
  (define data
    (match-lambda ((struct c/Data (n c)) (cons (typePredicate n) (foldl append null (map (curry constructor n) c))))))
  
  (define (field constructorName fieldName)
    `(define ,(toSymbol "variable/" fieldName)
       (delay (lambda (x) (force (,(toSymbol "constructor/" constructorName "-" fieldName) (force x)))))))
  
  (define (compileModule syntax types)
    (match syntax
      ((struct c/Module (n e i d))
       (let ((datas (filter c/Data? d))
             (decls (filter c/Declaration? d)))
         `(module ,(toSymbol n) scheme
            (define (coffer)
              (define-struct haskell/Coffer (value))
              (list make-haskell/Coffer haskell/Coffer-value haskell/Coffer?))
            ,@(map importRequire i)
            ,@(map importDefinition i)
            ,@(foldl append null (map data datas))
            ,@(map moduleDeclaration decls)
            ,@(map export e))))))
  
  (define compileSyntax
    (match-lambda 
      ((struct c/Application (r d)) `(,(compileSyntax r) (delay ,(compileSyntax d))))
      ((struct c/Character (v)) `(Char# ,(string-ref v 0)))
      ((struct c/Float (v)) `(Float# ,(string->number v)))
      ((struct c/Function (p b)) `(lambda (,(toSymbol "variable/" p)) ,(compileSyntax b)))
      ((struct c/If (g t e)) `(if (equal? ,(compileSyntax g) (force variable/Haskell.True)) ,(compileSyntax t) ,(compileSyntax e)))
      ((struct c/Integer (v)) `(Int# ,(string->number v)))
      ((struct c/Let (d b)) `(letrec ,(map letDeclaration d) ,(compileSyntax b)))
      ((struct c/ListConstructor ()) '(force variable/Haskell.Nil#))
      ((struct c/TupleConstructor (a)) (tupleConstructor a))
      ((struct c/UnitConstructor ()) '(force variable/Haskell.Unit#))
      ((struct c/Variable (n)) `(force ,(toSymbol "variable/" n)))))
  
  (define declarationExport
    (match-lambda
      ((struct c/Declaration (l _)) (list (string-append "variable/" l) l))))
  
  (define export
    (match-lambda
      ((struct c/Export (n)) `(provide (rename-out (,(toSymbol "variable/" n) ,(toSymbol n)))))))
  
  (define importDefinition
    (match-lambda
      ((struct c/Import (l m n t))
       (let* ((qualifiedName (string-append m "." n))
              (importQualifiedName (toSymbol "import/" qualifiedName)))
         `(define ,(toSymbol "variable/" qualifiedName)
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
       `(,(toSymbol "variable/" l) (delay ,(compileSyntax r))))))
  
  (define moduleDeclaration
    (match-lambda
      ((struct c/Declaration (l r)) `(define ,(toSymbol "variable/" l) (delay ,(compileSyntax r))))))
  
  (define toSymbol
    (lambda s (string->symbol (foldl (lambda (x y) (string-append y x)) "" s))))
  
  #;(define toSyntax
      (lambda s `,(string->symbol (foldl (lambda (x y) (string-append y x)) "" s))))
  
  (define (tupleConstructor arity)
    (let ((vars (map (lambda (x) (toSymbol "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 arity))))
      `(curry (lambda ,vars (list ,@vars)))))
  
  (define (typePredicate type)
    `(define ,(toSymbol type "?") "TODO"))
  
  (define (typePredicateExport typeName)
    `(provide ,(toSymbol typeName "?"))))