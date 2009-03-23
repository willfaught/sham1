; This file contains a reader for a subset of OCaml that is able to share higher-order, polymorphic functions with mzscheme, while preserving parametricity
; By David Kinghorn
;; now by me too! (JBC, 2007-06-19)

; TODO: get tuples and nested tuples to work properly
; TODO: do lots of testing
; TODO: Add units and variants to the language.

(module ml-reader mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           (lib "list.ss")
           "stack.ss"
           "ml-expander.ss"
           "ml-typechecker.ss")
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (only (lib "list.ss" "srfi" "1") lset=))
  
  (provide ml-read-syntax
           unification-test-suite)
  
   
  
  (define-tokens regular (ID LITNUM TYVAR FILENAME LITSTRING CAPID))
  (define-empty-tokens keywords (EQUALS IN LET REC SEMICOLON COLON-G LUMP INT STRING ARROW IF THEN ELSE PLUS MINUS STAR LPAREN RPAREN DIVIDE FILE 
                                        HASH-SCHEME TRUE FALSE BOOL AND OR NOT NE GE GT LE LT MODULE INCLUDE MOD FUN FUNCTION COMMA 
                                        TYPE PIPE OF
                                        EOF))
 
  
  (define-lex-abbrevs
    (lower-letter (:/ "a" "z"))
    (upper-letter (:/ #\A #\Z))
    (litnum-start (:or "+" "-" digit))
    (digit (:/ "0" "9"))    
    (id-cont (:or lower-letter upper-letter digit "_" "-"))
    (file-cont (:or lower-letter upper-letter digit "_" "-" "."))    
    (comment-start "(*")
    (comment-end "*)"))
  
  (define simple-lexer
    (lexer-src-pos
     [whitespace (return-without-pos (simple-lexer input-port))]
     [comment-start (return-without-pos ((comment-lexer 1) input-port))]
     ;; TODO: signal a read error here:
     [comment-end (error 'simple-lexer "End of comment outside of comment block.")]
     ["=" (token-EQUALS)]
     [";" (token-SEMICOLON)]
     [":G" (token-COLON-G)]
     ["->" (token-ARROW)]
     ["+" (token-PLUS)]
     ["-" (token-MINUS)]
     ["*" (token-STAR)]
     ["/" (token-DIVIDE)]
     ["(" (token-LPAREN)]
     [")" (token-RPAREN)]
     ["<>" (token-NE)]
     [">=" (token-GE)]
     [">" (token-GT)]
     ["<=" (token-LE)]
     ["<" (token-LT)]
     ["&" (token-AND)]
     ["," (token-COMMA)]
     ["|" (token-PIPE)]
     ["#include" (token-INCLUDE)]
     ["#module" (token-MODULE)]     
     [(:: lower-letter (:* id-cont)) (keyword-filter lexeme)]
     [(:: upper-letter (:* id-cont)) (token-CAPID lexeme)]
     [(:: "'" (:or lower-letter upper-letter) (:* id-cont)) (token-TYVAR lexeme)]     
     [(:: litnum-start (:* digit)) (token-LITNUM lexeme)]     
     ;; disallow strings with embedded double-quotes, for now.
     [(::  #\" (:* (:~ #\")) #\")
      (token-LITSTRING (substring lexeme 1 (- (string-length lexeme) 1)))]
     [(eof) (token-EOF)]))
  
  ; Enable nested comments
  (define (comment-lexer level)
    (lexer
     [comment-start ((comment-lexer (+ level 1)) input-port)]
     [comment-end (if (= level 1) (simple-lexer input-port) ((comment-lexer (- level 1)) input-port))]     
     [(:or (:~ whitespace) whitespace) ((comment-lexer level) input-port)]
     [(eof) (error 'comment-lexer "End of file occured inside a comment.")]))
  
  ;; keyword-filter : string -> token
  (define (keyword-filter str)
    (let ([maybe-kwd (assoc str keyword-list)])
      (if maybe-kwd
          ((cadr maybe-kwd))
          (token-ID str))))
  
  (define keyword-list
    `(("let" ,token-LET) ("in" ,token-IN) ("if" ,token-IF) ("then" ,token-THEN) ("else" ,token-ELSE) ("lump" ,token-LUMP) ("int" ,token-INT) 
                         ("string" ,token-STRING)                         
                         ("rec" ,token-REC) ("file" ,token-FILE) ("true" ,token-TRUE) ("false" ,token-FALSE) ("bool" ,token-BOOL) 
                         ("not" ,token-NOT) ("or" ,token-OR) ("mod" ,token-MOD) ("fun" ,token-FUN) ("function" ,token-FUNCTION)
                         ("type" ,token-TYPE) ("of" ,token-OF)))
  
  (print-struct #t)
  
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))  
  
  (define-syntax (build-so stx)
    (syntax-case stx ()
      ((_ value start end)
       (with-syntax ((start-pos (datum->syntax-object 
                                 (syntax end)
                                 (string->symbol 
                                  (format "$~a-start-pos"
                                          (syntax-object->datum (syntax start))))))
                     (end-pos (datum->syntax-object 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax-object->datum (syntax end))))))
                     (source (datum->syntax-object
                              (syntax end)
                              'source-name)))
         (syntax
          (datum->syntax-object 
           #f
           value
           (list source 
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))
           stx-for-original-property))))))
  
  
  
  (define (ml-parser source-name)
    (parser (src-pos) 
            (start program)
            (end EOF)
            (tokens regular keywords)
            
            (error (lambda (a name val start end)
                     (raise-read-error 
                      "parse error"
                      source-name
                      (position-line start)
                      (position-col start)
                      (position-offset start)
                      (- (position-offset end)
                         (position-offset start)))))    
            
            (grammar 
             (program 
              [(module includes defs) (make-program (build-so $1 1 3) $1 $2 $3)])
             (module
              [(MODULE LPAREN LITSTRING RPAREN) (string->symbol $3)])           
             (includes
              [(INCLUDE LPAREN files RPAREN) $3])
             (files
              [(file files) (cons $1 $2)]
              [() null])
             (file
              [(LITSTRING) $1])
             (defs
               [(defn defs) (cons $1 $2)]
               [() null])
             (defn
               [(LET REC id params EQUALS exp SEMICOLON SEMICOLON) (make-let-def (build-so $3 1 7) $3 $4 $6 #t)]
               [(LET id params EQUALS exp SEMICOLON SEMICOLON) (make-let-def (build-so $2 1 6) $2 $3 $5 #f)]
               [(TYPE id EQUALS constr-decls SEMICOLON SEMICOLON) (make-typedef (build-so $2 1 4) $2 $4)])
             (constr-decls
              [(constr-decl PIPE constr-decls) (cons $1 $3)]
              [(constr-decl) (list $1)])
             (constr-decl
              [(CAPID) (make-constr-decl/0ary (build-so $1 1 1) $1)]
              [(CAPID OF type) (make-constr-decl (build-so $1 1 3) $1 $3)])
             (params
              [(param params) (cons $1 $2)]
              [() null])
             (param 
              [(param-tuple-list) (if (null? (cdr $1)) (car $1) (make-tuple (build-so $1 1 1) $1))])
             (param-tuple-list
              [(simple-param COMMA param-tuple-list) (cons $1 $3)]
              [(simple-param) (list $1)])             
             (simple-param
              [(id) $1]
              [(LPAREN param RPAREN) $2])
             (exp 
              [(LET REC id params EQUALS exp IN exp) (make-let-exp (build-so $3 1 8) $3 $4 $6 $8 #t)]
              [(LET id params EQUALS exp IN exp) (make-let-exp (build-so $2 1 7) $2 $3 $5 $7 #f)]              
              [(FUNCTION param ARROW exp) (make-anonymous-function (build-so $2 1 4) (list $2) $4)]
              [(FUN param ARROW exp) (make-anonymous-function (build-so $2 1 4) (list $2) $4)]
              [(IF exp THEN exp ELSE exp) (make-if-exp (build-so $2 1 6) $2 $4 $6)]
              [(id COLON-G type) (make-ms (build-so $1 1 3) $1 (replace-unbound-tyvars-with-fresh-tyvars $3))]
              [(tuple-exp) $1])
             (tuple-exp
              [(tuple-list) (if (null? (cdr $1)) (car $1) (make-tuple (build-so $1 1 1) $1))])
             (tuple-list
              [(logical-exp COMMA tuple-list) (cons $1 $3)]
              [(logical-exp) (list $1)])
             (logical-exp
              [(NOT comparison-exp) (make-app (build-so $2 1 2) (anon-ms (anon-id 'not) (anon-arrow anon-bool anon-bool)) (list $2))]
              ; And and Or need to be short circuited, so they are converted to conditional expressions rather than Scheme Imports              
              [(comparison-exp OR comparison-exp) (make-if-exp (build-so $1 1 3) $1 (make-bool #f #t) $3)]
              [(comparison-exp AND comparison-exp) (make-if-exp (build-so $1 1 3) $1 $3 (make-bool #f #f))]           
              [(comparison-exp) $1])
             (comparison-exp
              ; I would like to do the following, but I can't because the import hides the actual arguments from Scheme
              ; Create an import that doesn't do any checking? I can't just change the type of the arguments because I want ML's typechecker run on the two arguments first
              ; [(mul-exp EQUALS mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-equals) (anon-arrow (anon-tyvar 'a) (anon-tyvar 'a) anon-bool)) (list $1 $3))]
              ; [(mul-exp NE mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-ne) (anon-arrow (anon-tyvar 'a) (anon-tyvar 'a) anon-bool)) (list $1 $3))]
              ;; JBC: okay, I introduced a prim term to handle this.
              [(mul-exp EQUALS mul-exp) (make-app (build-so $1 1 3) (make-prim #f 'equals) (list $1 $3))]
              [(mul-exp NE mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-ne) (anon-arrow anon-int anon-int anon-bool)) (list $1 $3))]
              [(mul-exp GE mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-ge) (anon-arrow anon-int anon-int anon-bool)) (list $1 $3))]              
              [(mul-exp GT mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-gt) (anon-arrow anon-int anon-int anon-bool)) (list $1 $3))]
              [(mul-exp LE mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-le) (anon-arrow anon-int anon-int anon-bool)) (list $1 $3))]
              [(mul-exp LT mul-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-lt) (anon-arrow anon-int anon-int anon-bool)) (list $1 $3))]              
              [(mul-exp) $1])
             (mul-exp
              [(add-exp STAR add-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-times) (anon-arrow anon-int anon-int anon-int)) (list $1 $3))] 
              [(add-exp DIVIDE add-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-divide) (anon-arrow anon-int anon-int anon-int)) (list $1 $3))]
              [(add-exp MOD add-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-modulo) (anon-arrow anon-int anon-int anon-int)) (list $1 $3))]
              [(add-exp) $1])
             (add-exp 
              [(app-exp PLUS app-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-plus) (anon-arrow anon-int anon-int anon-int)) (list $1 $3))] 
              [(app-exp MINUS app-exp) (make-app (build-so $1 1 3) (anon-ms (anon-id 'ml-minus) (anon-arrow anon-int anon-int anon-int)) (list $1 $3))]
              [(MINUS app-exp) (make-app (build-so $2 1 2) (anon-ms '- (anon-arrow anon-int anon-int)) (list $2))]              
              [(app-exp) $1])
             (app-exp
              [(simple-exp simple-exp args) (make-app (build-so $1 1 3) $1 (cons $2 $3))]
              [(simple-exp) $1])
             (args
              [(simple-exp args) (cons $1 $2)]
              [() null])
             (simple-exp
              [(id) $1]
              [(CAPID) (make-constr (build-so $1 1 1) $1)]
              [(LITNUM) (make-litnum (build-so $1 1 1) (string->number $1))]
              [(TRUE) (make-bool #f #t)]
              [(FALSE) (make-bool #f #f)]
              [(LPAREN RPAREN) anon-unit-term]
              [(LPAREN exp RPAREN) $2]
              [(LITSTRING) (make-litstr (build-so $1 1 1) $1)])
             (id [(ID) (make-id (build-so $1 1 1) (string->symbol $1))])
             (type
              [(tuple-type-list) (if (null? (cdr $1)) (car $1) (make-tuple-type (build-so $1 1 1) $1))])
             (tuple-type-list
              [(arrow-type STAR tuple-type-list) (cons $1 $3)]
              [(arrow-type) (list $1)])
             (arrow-type
              [(simple-type ARROW arrow-type) (make-arrow-type (build-so $1 1 3) $1 $3)] ; Right associative
              [(simple-type) $1])
             (simple-type 
              [(INT) (make-int-type #f)]
              [(STRING) (make-str-type #f)]
              [(BOOL) (make-bool-type #f)]
              [(LUMP) (make-lump-type #f)]
              [(TYVAR) (make-tyvar (build-so $1 1 1) (string->symbol $1))]
              [(LPAREN type RPAREN) $2]
              [(LPAREN RPAREN) anon-unit-type]
              [(ID) (make-user-type (build-so $1 1 1) $1)]))))
  
  
 

  ;;
  ;; READ
  ;;

  
  (define (ml-read-syntax source-name input-port)
    (port-count-lines! input-port)
    (let ([program ((ml-parser source-name) (lambda () (simple-lexer input-port)))])
      (let* ([defs (program-defs program)] 
             [typemap (make-hash-table)]
             [usertypes (filter typedef? defs)]
             [let-defs (filter not-typedef? defs)])
        (collision-check usertypes)
        (let ([types (map                       
                      (lambda (def) 
                        (let ([css (new-stack)])
                          (stack-push! css (new-stack))
                          (let ([type ((typecheck usertypes) def typemap css)])
                            (let ([replacements (unify (stack-peek css) (make-hash-table))])
                              (hash-table-for-each replacements 
                                                   (lambda (tyvar new-type)
                                                     (set! type (tyrep type tyvar new-type))))
                              (hash-table-put! typemap (id-val (let-def-id def)) type)
                              type))))
                      let-defs)])
          (let ([code 
                 (let ([defn (map emit-code let-defs types)])
                   #`(module #,(program-module program) mzscheme
                       (require (lib "contract.ss")
                                (lib "ml-primitives.ss" "smootxes")
                                #,@(map (lambda (include) (list 'file include)) (program-includes program)))      
                       (provide (all-defined))
                       #,@defn))])
            ;; display the module source:
            #;(printf "~v\n" (syntax-object->datum code))
            code)))))
  
      
  (define (not-typedef? x) (not (typedef? x)))

  
  ;;; TESTING
  
  
  
  ;; UNIFICATION TESTS
  
  (define (ut . constraint-list)
    (let ([ht (make-hash-table)])
      (unify (list->stack (map (lambda (x) (apply make-constraint x)) constraint-list)) ht)
      (hash-table-map ht list)))
  
  ;; my test function. 
  (define (ls= x y) (lset= equal? x (map (lambda (pr) (list (tyvar-val (car pr)) (cadr pr))) y)))
  
  
  ;; I'm suspicious about the lack of anon-tuple
  (define (anon-tuple . types)
    (make-tuple-type #f types))
  
  (define int1 (make-int-type 34))
  (define int2 (make-int-type 37))
  (define bool1 (make-bool-type 2987))
  (define bool2 (make-bool-type 223))
  (define (arr1 t1 t2) (make-arrow-type 327 t1 t2))
  (define (arr2 t1 t2) (make-arrow-type 272987 t1 t2))
  (define (tup1 . ts) (make-tuple-type 232 ts))
  (define (tup2 . ts) (make-tuple-type 2132 ts))
  (define tv1a (make-tyvar 23 79))
  (define tv1b (make-tyvar 124 79))
  (define tv2 (make-tyvar 234 81))
  (define tv3 (make-tyvar 12 2))
  
  
  (define unification-test-suite
    (test-suite
     "unification tests"
     ;; single constraint:
     
     ;; simple types
     (test-check "trivial" ls= (ut) '())
     (test-check "success1" ls= (ut (list int1 int2)) '())
     (test-check "success2" ls= (ut (list bool2 bool1)) '())
     (test-exn "simple-fail" exn:fail? (lambda () (ut (list bool1 int1))))
     
     ;; arrows
     (test-check "success3" ls= (ut (list (arr1 int1 int2)
                                          (arr2 int2 int1)))
                 '())

     (test-exn "fail2" exn:fail? (lambda ()
                                   (ut (list (anon-arrow anon-bool anon-int) (anon-arrow anon-int anon-int)))))
     (test-exn "fail3" exn:fail? (lambda ()
                                   (ut (list (anon-arrow anon-bool anon-int) (anon-arrow anon-bool anon-bool)))))
     
     ;; tuples
     (test-check "tuple1" ls= (ut (list (anon-tuple) (anon-tuple))) '())
     (test-check "tuple2" ls= (ut (list (anon-tuple anon-int anon-bool) (anon-tuple anon-int (make-bool-type 2387)))) '())
     (test-exn "tuple-fail" exn:fail? (lambda () (ut (list (anon-tuple anon-int) (tup1 bool2)))))
     (test-exn "tuple-fail2" exn:fail? (lambda () (ut (list (tup1 bool1 bool2 int2) (tup2 bool2 int1 int2)))))
     
     ;; live vars, now:
     
     ;; top level
     (test-check "var1" ls= (ut (list tv1a int2)) `((,tv1a ,int2)))
     (test-check "var2" ls= (ut (list int2 tv1a)) `((,tv1a ,int2)))
     (test-check "var3" ls= (ut (list tv1a tv1b)) '())
     ;; arrows
     (test-check "var4" ls= (ut (list (arr1 tv2 bool1) (arr2 int2 tv3))) `((,tv2 ,int2) (,tv3 ,bool1)))
     (test-check "var4" ls= (ut (list (arr1 tv2 tv1a) (arr2 int2 tv3))) `((,tv2 ,int2) (,tv1a ,tv3))) ;; could go either way
     ;; tuples
     (test-check "vartup" ls= (ut (list (tup1 int1 tv2 bool1)
                                        (tup2 tv1a (arr2 int2 tv3) tv3)))
                 `((,tv1a ,int1)
                   (,tv2 ,(arr2 int2 bool1))
                   (,tv3 ,bool1)))
     
     
     (test-check "tpair" ls= (ut (list (arr1 tv2 tv3) (arr2 tv3 tv2))) `((,tv3 ,tv2)))
     (test-exn "trecursive" exn:fail? (lambda () (ut (list tv1a (arr1 int1 tv1a)))))
     
     
    
  ))

  )