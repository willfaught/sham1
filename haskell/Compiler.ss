(module Compiler mzscheme
  (require (only (lib "1.ss" "srfi") partition)
           (only (lib "71.ss" "srfi") values->list)
           (only (lib "list.ss") foldl)
           (only (lib "match.ss") match match-lambda match-let)
           (only (lib "Converters.ss" "sham") convertHM convertHS)
           (prefix c/ (lib "CoreSyntax.ss" "sham" "haskell"))
           (prefix h/ (lib "HaskellSyntax.ss" "sham" "haskell"))
           (only (lib "List.ss" "sham" "haskell") iterate)
           (only (lib "TypeChecker.ss" "sham" "haskell") moduleContext)
           (prefix t/ (lib "Types.ss" "sham")))
  
  (provide compileCS)
  
  ; compileConstructor :: string string [string] -> datum
  (define (compileConstructor dataName constructorName fieldNames)
    (let ((t (stringsToSymbol "haskell/type/" dataName))
          (c (stringsToSymbol "haskell/constructor/" constructorName))
          (f (map (lambda (x) (string->symbol x)) fieldNames)))
      `(define-struct (,c ,t) ,f #f)))
  
  ; compileCS :: c/CoreSyntax -> datum
  (define (compileCS syntax)
    (match syntax
      (($ c/Application r d) `(,(compileCS r) (delay ,(compileCS d))))
      (($ c/Character v) (string-ref v 0))
      (($ c/Data n c) (compileData n c))
      (($ c/Float v) (string->number v))
      (($ c/Function p b) `(lambda (,(string->symbol (string-append "haskell/" p))) ,(compileCS b)))
      (($ c/If g t e) `(if (equal? ,(compile g) (force haskell:True)) ,(compile t) ,(compileCS e)))
      (($ c/Integer v) (string->number v))
      (($ c/Let d b) (compileLet d b))
      (($ c/ListConstructor) 'null)
      ((? c/ML? x) (compileML x))
      (($ c/Module n i d) (compileModule n i d))
      (($ c/Scheme t n) `(contract ,(contractHS t) ,(convertHS t (string->symbol n) 1) 'scheme 'haskell))
      (($ c/TupleConstructor a) (compileTupleConstructor a))
      (($ c/UnitConstructor) `(vector-immutable))
      (($ c/Variable n) `(force ,(string->symbol (string-append "haskell/" n))))))
  
  ; compileCurriedConstructor :: string integer -> datum
  (define (compileCurriedConstructor name arity)
    `(define ,(stringsToSymbol "haskell/" name)
       (delay ,(nest `(,(stringsToSymbol "make-haskell/constructor/" name)
                       ,@(map (lambda (x) (stringsToSymbol "x" x))
                              (iterate (lambda (x) (+ x 1)) 1 arity))) 1 arity))))
  
  ; compileData : string [c/Constructor] -> [datum]
  (define (compileData name constructors)
    (foldl append null (map (match-lambda (($ c/Constructor n1 f) (append (list (compileType name)
                                                                                (compileConstructor name n1 (map (match-lambda (($ c/Field n2 _) n2)) f))
                                                                                (compileCurriedConstructor n1 (length f))
                                                                                (compilePredicate n1))
                                                                          (map (match-lambda (($ c/Field n2 _) (compileField n1 n2))) f))))
                            constructors)))
  
  ; compileField :: string string -> datum
  (define (compileField constructorName fieldName)
    `(define (stringsToSymbol "haskell:" fieldName)
       (delay (lambda (x) (force (,(string->symbol (string-append "haskell/constructor/" constructorName "-" fieldName)) (force x)))))))
  
  ; compileLet :: c/Let -> datum
  (define (compileLet declarations body)
    ; compileDeclaration :: c/Declaration -> datum
    (define compileDeclaration
      (match-lambda (($ c/Declaration n b) `(,(string->symbol (string-append "haskell/" n)) (delay ,(compileCS b))))))
    `(letrec ,(map compileDeclaration declarations) ,(compileCS body)))
  
  ; compileML :: c/ML -> datum
  (define (compileML type name) 'TODO)
  
  ; compileModule :: string [string] [c/Declaration] -> datum
  (define (compileModule name imports declarations)
    ; compileDeclaration :: c/Declaration -> datum
    (define compileDeclaration
      (match-lambda (($ c/Declaration l r) `(define ,(string->symbol l) (delay ,(compileCS r))))))
    (match-let (((types bindings) (values->list (partition c/Data? declarations))))
      `(module ,(string->symbol name) mzscheme
         (require (only (lib "1.ss" "srfi") circular-list? proper-list?)
                  (only (lib "contract.ss") -> flat-contract)
                  (only (lib "list.ss") foldl foldr)
                  (lib "Primitives.ss" "sham" "haskell")
                  (lib "Types.ss" "sham")
                  ,@(map (lambda (x) `(file ,x)) imports))
         (provide ,@(map (match-lambda ((name _) name)) (moduleContext declarations)))
         ,@(foldl append null (map compileCS types))
         ,@(map compileDeclaration bindings))))
  
  ; compilePredicate :: string -> datum
  (define (compilePredicate name)
    (let ((n (stringsToSymbol "haskell/is" name))
          (b `(delay (lambda (x)
                       (if (,(string->symbol (string-append "haskell/constructor/" name "?")) (force x))
                           (force haskell:True)
                           (force haskell:False))))))
      `(define ,n ,b)))
  
  ; compileTupleConstructor :: integer -> datum
  (define (compileTupleConstructor arity)
    (nest `(vector-immutable ,@(map (lambda (x) (stringsToSymbol "x" (number->string x))) (iterate (lambda (x) (+ x 1)) 1 arity))) 1 arity))
  
  ; compileType :: string -> datum
  (define (compileType typeName)
    `(define-struct ,(stringsToSymbol "haskell/type/" typeName) () #f))
  
  ; contractHS :: h/HaskellSyntax -> contract
  (define (contractHS syntax)
    (match syntax
      (($ h/FunctionType p r) `(-> ,(contractSH p) ,(contractHS r)))
      (($ h/ListType t) `(and/c (listof ,(contractHS t)) (flat-contract proper-list?) (flat-contract (lambda (x) (not (circular-list? x))))))
      (($ h/TupleType t) `(vector-immutable/c ,@(map contractHS t)))
      (($ h/TypeConstructor "Bool") `(flat-contract boolean?))
      (($ h/TypeConstructor "Char") `(flat-contract char?))
      (($ h/TypeConstructor "Float") `(flat-contract number?))
      (($ h/TypeConstructor "Int") `(flat-contract integer?))
      (($ h/TypeConstructor n) `(flat-contract ,(string->symbol (string-append "haskell:" n "Type?")))) ;TODO
      ((? h/TypeVariable? _) 'any/c) ;TODO
      (($ h/UnitType) `(vector-immutable/c))))
  
  ; contractSH :: h/HaskellSyntax -> contract
  (define (contractSH syntax)
    (match syntax
      (($ h/FunctionType p r) `(-> ,(contractHS p) ,(contractSH r)))
      ((? (lambda (x) (or h/ListType? h/TupleType? h/TypeConstructor? h/TypeVariable? h/UnitType?)) _) 'any/c)))
  
  ; nest :: datum integer integer -> datum
  (define (nest syntax from to)
    (if (equal? from to) syntax `(lambda (,(stringsToSymbol "x" (number->string from))) ,(nest syntax (+ from 1) to))))
    
  ; stringsToSymbol :: string string -> symbol
    (define (stringsToSymbol s1 s2)
    (string->symbol (string-append s1 s2))))