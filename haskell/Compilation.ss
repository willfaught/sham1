(module Compilation scheme
  (require (lib "Boundary.ss" "sham")
           (prefix-in c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix-in h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (lib "List.ss" "sham" "haskell")
           (lib "Maybe.ss" "sham" "haskell")
           (prefix-in t/ (lib "Types.ss" "sham"))
           (lib "TypeChecking.ss" "sham" "haskell"))
  
  (provide compileModule compileExpression)
  
  ; Modules
  
  (define (compileModule syntax types)
    (checkBindingConflicts types)
    (match syntax
      ((struct c/Module (n e i d))
       (let ((datas (filter c/Data? d))
             (decls (filter c/Declaration? d)))
         `(module ,(toSymbol (typeName n)) scheme
            (require (lib "Primitives.ss" "sham" "haskell"))
            ,@(map importData i)
            ,@(remove-duplicates (foldl append null (map importTypes i)))
            ,@(map exportDataH datas)
            ,@(map exportDeclaration e)
            ,@(map (curry importDefinition n) i)
            ,@(foldl append null (map data datas))
            ,@(map dataContractH datas)
            ,@(map moduleDeclaration decls))))))
  
  (define (checkBindingConflicts types)
    (let ((dups (duplicates (map (match-lambda ((struct Assumption (n _)) n)) types))))
      (if (> (length dups) 0)
          (error 'checkBindingConflicts (format "There are conflicting names: ~a" dups)) #f)))
  
  (define (duplicates xs)
    (let ((pair (foldl (lambda (x y) (if (elem x (first y)) y (if (elem x (second y))
                                                      (list (cons x (first y)) (second y))
                                                      (list (first y) (cons x (second y)))))) (list null null) xs)))
      (first pair)))
  
  (define (importRequire item)
    `(require ,item))
  
  (define (importRequireOnly library item)
    (importRequire `(only-in ,library ,item)))
  
  (define (importLibrary path)
    (let ((paths (splitPath path)))
    `(lib ,(string-append (last paths) ".ss") "sham" "modules" ,@(drop-right paths 1))))
  
  (define (splitPath path)
    (regexp-split #rx"\\." path))
  
  (define importData
    (match-lambda
      ((struct c/Import (l m n _)) (importRequireOnly (importLibrary m) `(,(toSymbol n) ,(toSymbol "import/" m "." n))))))
  
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
  
  (define (importRename language type)
    (let ((name (typeName type))
          (module (typeModule language type)))
      `(,(toSymbol name "/" language) ,(toSymbol "type/" module "." name "/" language))))
  
  (define (typeToString language type)
    (match type
      ((struct t/Application (_ _)) (error 'typeToString "Unexpected type application."))
      ((struct t/Constructor (n)) n)
      ((struct t/Function ()) (error 'typeToString "Unexpected function type constructor."))
      ((struct t/List ()) (match language
                            ("haskell" "List")
                            #;("ml" 'TODO)
                            #;("scheme" 'TODO)))
      ((struct t/Tuple (_)) (match language
                              ("haskell" "Tuple#")
                              #;("ml" 'TODO)
                              #;("scheme" 'TODO)))
      ((struct t/Unit ()) (match language
                            ("haskell" "Unit")
                            #;("ml" 'TODO)
                            #;("scheme" 'TODO)))
      ((struct t/Variable (_)) (error 'typeToString "Unexpected type variable."))))
  
  (define (importType language type)
    (let ((name (typeToString language type)))
      (importRequireOnly (importLibrary (typeModule language name))
                         (importRename language name))))
  
  (define importTypes
    (match-lambda
      ((struct c/Import (l _ _ t))
       (let ((qtycons (match-lambda
                        ((struct t/Function ()) #f)
                        ((struct t/Tuple (_)) #f)
                        (_ #t))))
         (map (curry importType l)
              (remove-duplicates (filter qtycons (t/typeConstructors t))))))))
  
  (define (importDefinition module import)
    (match import
      ((struct c/Import (l m n t))
       (let* ((qname (string-append m "." n))
              (prefixed (toSymbol "import/" qname)))
         `(define ,(toSymbol "variable/" qname)
            ,(match l
               ("haskell" (boundaryHH t prefixed module m))
               #;("ml" 'TODO)
               #;("scheme" 'TODO)))))))
  
  (define moduleDeclaration
    (match-lambda
      ((struct c/Declaration (l r)) `(define ,(toSymbol "variable/" l) (delay ,(compileExpression r))))))
  
  (define exportDeclaration
    (match-lambda
      ((struct c/Export (n)) `(provide (rename-out (,(toSymbol "variable/" n) ,(toSymbol n)))))))
  
  (define exportDataH
    (match-lambda
      ((struct c/Data (n _ _)) `(provide (rename-out (,(toSymbol "type/" n "/haskell") ,(toSymbol n "/haskell")))))))
  
  ; Data contracts
  
  (define dataContractH
    (match-lambda
      ((struct c/Data (n t c)) 
       (let ((body `(promise/c (recursive-contract (or/c ,@(map constructorContractH c))))))
         `(define ,(toSymbol "type/" n "/haskell")
            ,(if (null? t) body `(curry (lambda ,(map (lambda (x) (toSymbol "variable/" x)) t) ,body))))))))
  
  (define constructorContractH
    (match-lambda
      ((struct c/Constructor (n f)) `(,(toSymbol "constructor/" n "/c") ,@(map fieldContractH f)))))
  
  (define fieldContractH
    (match-lambda
      ((struct c/Field (_ t)) `(promise/c ,(contractH t)))))
  
  ; Data
  
  (define constructorPredicate
    (match-lambda
      ((struct c/Constructor (n _))
       `(define ,(toSymbol "variable/is" n)
          (delay (lambda (x)
                   (if (,(toSymbol "constructor/" n "?") (force x))
                       (force variable/Haskell.Prelude.True)
                       (force variable/Haskell.Prelude.False))))))))
  
  (define (constructorDefinition constructor)
    (match-let (((struct c/Constructor (n f)) constructor))
      (let ((cs (toSymbol "constructor/" n))
            (fs (map (lambda (x) (toSymbol x)) (map (match-lambda ((struct c/Field (n _)) n)) f))))
        `(define-contract-struct ,cs ,fs))))
  
  (define (constructor syntax)
    (let ((type (constructorDefinition syntax))
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
      ((struct c/Data (n _ c)) (foldl append null (map constructor c)) #;(cons (dataContractH n) ))))
  
  (define (field constructorName fieldName)
    `(define ,(toSymbol "variable/" fieldName)
       (delay (lambda (x) (force (,(toSymbol "constructor/" constructorName "-" fieldName) (force x)))))))
  
  ; Expressions
  
  (define compileExpression
    (match-lambda 
      ((struct c/Application (r d)) `(,(compileExpression r) (delay ,(compileExpression d))))
      ((struct c/Character (v)) `(make-constructor/Char# ,(string-ref v 0)))
      ((struct c/Float (v)) `(make-constructor/Float# ,(string->number v)))
      ((struct c/Function (p b)) `(lambda (,(toSymbol "variable/" p)) ,(compileExpression b)))
      ((struct c/If (g t e)) `(if (equal? ,(compileExpression g) (force variable/Haskell.Prelude.True)) ,(compileExpression t) ,(compileExpression e)))
      ((struct c/Integer (v)) `(make-constructor/Int# ,(string->number v)))
      ((struct c/Let (d b)) `(letrec ,(map letDeclaration d) ,(compileExpression b)))
      ((struct c/ListConstructor ()) '(force variable/Haskell.Prelude.Nil))
      ((struct c/TupleConstructor (a)) (let ((vars (map (lambda (x) (toSymbol "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 a))))
                                         `(curry (lambda ,vars (make-constructor/Tuple# (list ,@vars))))))
      ((struct c/UnitConstructor ()) 'make-constructor/Unit)
      ((struct c/Variable (n)) `(force ,(toSymbol "variable/" n)))))
  
  (define letDeclaration
    (match-lambda
      ((struct c/Declaration (l r))
       `(,(toSymbol "variable/" l) (delay ,(compileExpression r))))))
  
  ; Miscellaneous
  
  (define toSymbol
    (lambda s (string->symbol (foldl (lambda (x y) (string-append y x)) "" s))))
  
  #;(define toSyntax
      (lambda s `,(string->symbol (foldl (lambda (x y) (string-append y x)) "" s)))))