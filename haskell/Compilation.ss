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
    (match syntax
      ((struct c/Module (n e i d))
       (let ((datas (filter c/Data? d))
             (decls (filter c/Declaration? d)))
         `(module ,(toSymbol n) scheme
            (require (lib "Primitives.ss" "sham" "haskell"))
            ,@(map importData i)
            ,@(remove-duplicates (foldl append null (map importTypes i)))
            ,@(map exportDataH datas)
            ,@(map exportDeclaration e)
            ,@(map importDefinition i)
            ,@(foldl append null (map data datas))
            ,@(map dataContractH datas)
            ,@(map moduleDeclaration decls)
            )))))
  
  (define (importRequire item)
    `(require ,item))
  
  (define (importRequireOnly library item)
    (importRequire `(only-in ,library ,item)))
  
  (define (importLibrary modules)
    `(lib ,(string-append (last modules) ".ss") "sham" "modules" ,@(drop-right modules 1)))
  
  (define (splitModules qualifiedModule)
    (regexp-split #rx"\\." qualifiedModule))
  
  (define importData
    (match-lambda
      ((struct c/Import (l m n _)) (importRequireOnly (importLibrary (splitModules m)) `(,(toSymbol n) ,(toSymbol "import/" m "." n))))))
  
  (define (importModule language module type)
    (match type
      ((struct t/Application (_ _)) (error 'importType "Unexpected type application"))
      ((struct t/Constructor (_)) module)
      ((struct t/Variable (_)) (error 'importType "Unexpected type variable"))
      (_ (match language
           ("haskell" "Haskell.Prelude")
           #;("ml" 'TODO)
           #;("scheme" 'TODO)))))
  
  (define (importName language module type)
    (match type
      ((struct t/Constructor (n)) `(,(toSymbol n "/" language) ,(toSymbol "type/" module "." n "/" language)))
      ((struct t/Function ()) (error 'importType "Unexpected function type constructor"))
      ((struct t/List ()) (match language
                            ("haskell" `(List#/haskell type/Haskell.Prelude.List#/haskell))
                            #;("ml" 'TODO)
                            #;("scheme" 'TODO)))
      ((struct t/Tuple (_)) (match language
                              ("haskell" `(Tuple#/haskell type/Haskell.Prelude.Tuple#/haskell))
                              #;("ml" 'TODO)
                              #;("scheme" 'TODO)))
      ((struct t/Unit ()) (match language
                            ("haskell" `(Unit#/haskell type/Haskell.Prelude.Unit#/haskell))
                            #;("ml" 'TODO)
                            #;("scheme" 'TODO)))))
  
  (define (importType language module type)
    (importRequireOnly (importLibrary (splitModules (importModule language module type)))
                       (importName language module type)))
  
  (define importTypes
    (match-lambda
      ((struct c/Import (l m _ t)) (map (curry importType l m) (remove-duplicates (filter (lambda (x) (not (t/Function? x))) (t/typeConstructors t)))))))
  
  (define importDefinition
    (match-lambda
      ((struct c/Import (l m n t))
       (let* ((qualifiedName (string-append m "." n))
              (importQualifiedName (toSymbol "import/" qualifiedName)))
         `(define ,(toSymbol "variable/" qualifiedName)
            ,(match l
               ("haskell" `(,(boundaryHH t) ,importQualifiedName))
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
       (let ((body `(recursive-contract (or/c ,@(map constructorContractH c)))))
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
  
  (define (constructorDefinition typeName constructor)
    (match-let (((struct c/Constructor (n f)) constructor))
      (let ((cs (toSymbol "constructor/" n))
            (fs (map (lambda (x) (toSymbol x)) (map (match-lambda ((struct c/Field (n _)) n)) f))))
        `(define-contract-struct ,cs ,fs))))
  
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
      ((struct c/ListConstructor ()) '(force variable/Haskell.Prelude.Nil#))
      ((struct c/TupleConstructor (a)) (let ((vars (map (lambda (x) (toSymbol "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 a))))
                                         `(curry (lambda ,vars (make-constructor/Tuple# (list ,@vars))))))
      ((struct c/UnitConstructor ()) 'make-constructor/Unit#)
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