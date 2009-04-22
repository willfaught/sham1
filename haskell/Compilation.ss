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
  
  (define (data syntax)
    (match syntax
      ((struct c/Data (n _ c)) (foldl append null (map (curry constructor n) c)) #;(cons (dataContractH n) ))))
  
  (define contractH
    (match-lambda
      ((struct t/Application (r d)) `(,(contractH r) ,(contractH d)))
      ((struct t/Constructor (n)) (string->symbol (string-append n "/haskell/c")))
      ((struct t/Function ()) 'type/Haskell.Function#)
      ((struct t/List ()) 'type/Haskell.List#)
      ((struct t/Tuple (a)) `(type/Haskell.Tuple# ,a))
      ((struct t/Unit ()) 'type/Haskell.Unit#)
      ((struct t/Variable (n)) (string->symbol (string-append "haskell/" n)))))
  
  (define (dataContractH syntax)
    (define constructorContract
      (match-lambda
        ((struct c/Constructor (n f)) `(,(toSymbol "constructor/" n "/c") ,@(map fieldContract f)))))
    (define fieldContract
      (match-lambda
        ((struct c/Field (_ t)) `(promise/c ,(contractH t)))))
    (match-let* ((tyvars (remove-duplicates (dataTypeVariables syntax)))
                 ((struct c/Data (n _ c)) syntax)
                 (body `(recursive-contract (or/c ,@(map constructorContract c)))))
      `(define ,(toSymbol n "/haskell/c") 0)
      #;`(define ,(toSymbol n "/haskell/c")
         ,(if (null? tyvars)
             body
             `(curry (lambda ,(map (lambda (x) (toSymbol "haskell/" x)) tyvars) ,body))))))
  
  
  
  
  
  #;(define (List#? contract1)
      (recursive-contract (or/c (constructor/Nil#/c) (constructor/Cons#/c (promise/c contract1) (promise/c (List#? contract1))))))
  
  (define (dataPredicateExport typeName)
    `(provide ,(toSymbol typeName "?")))
  
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
              (define-struct Coffer (value))
              (list make-Coffer Coffer-value Coffer?))
            (require (lib "Primitives.ss" "sham" "haskell"))
            ,@(map importRequire i)
            ,@(map importDefinition i)
            ,@(foldl append null (map data datas))
            ,@(map moduleDeclaration decls)
            ,@(map export e))))))
  
  (define compileSyntax
    (match-lambda 
      ((struct c/Application (r d)) `(,(compileSyntax r) (delay ,(compileSyntax d))))
      ((struct c/Character (v)) `(variable/Char ,(string-ref v 0)))
      ((struct c/Float (v)) `(variable/Float ,(string->number v)))
      ((struct c/Function (p b)) `(lambda (,(toSymbol "variable/" p)) ,(compileSyntax b)))
      ((struct c/If (g t e)) `(if (equal? ,(compileSyntax g) (force variable/Haskell.True)) ,(compileSyntax t) ,(compileSyntax e)))
      ((struct c/Integer (v)) `(variable/Int ,(string->number v)))
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
               ("haskell" importQualifiedName #;(boundaryHH t importQualifiedName))
               #;("ml" importQualifiedName #;(boundaryHM t importQualifiedName))
               #;("scheme" importQualifiedName #;(boundaryHS t importQualifiedName))))))))
  
  (define importRequire
    (match-lambda
      ((struct c/Import (l m n _))
       (let ((modules (regexp-split #rx"\\." m)))
         `(require (only-in (lib ,(string-append (last modules) ".ss") "sham" "modules" ,@(drop-right modules 1))
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
  
  ; Data declaration type variables
  
  (define dataTypeVariables
    (match-lambda
      ((struct c/Data (_ _ c)) (foldl append null (map constructorTypeVariables c)))))
  
  (define constructorTypeVariables
    (match-lambda
      ((struct c/Constructor (_ f)) (foldl (lambda (x y)
                                             (match x
                                               ((struct Just (v)) (cons v y))
                                               ((struct Nothing ()) y)))
                                           null
                                           (map fieldTypeVariable f)))))
  
  (define fieldTypeVariable
    (match-lambda
      ((struct c/Field (_ (struct t/Variable (n)))) (make-Just n))
      ((struct c/Field (_ _)) (make-Nothing)))))