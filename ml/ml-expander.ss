(module ml-expander mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           (lib "contract.ss")
           (lib "ml-primitives.ss" "smootxes"))
  
  (provide (all-defined))
  
  (define-struct term (stx))
  
  ; The data in a term, may be any of the following. All fields except id will be a term.
  ; Note: all non-short-circuited operations have been turned into Scheme Imports.
  ; In order to add new operations just modify simple-parser  
  (define-struct (program term) (module includes defs))
  (define-struct (let-def term) (id params exp rec))
  (define-struct (let-exp term) (id params exp body rec))
  (define-struct (anonymous-function term) (params exp)) 
  (define-struct (app term) (fun args))
  (define-struct (id term) (val))
  (define-struct (ms term) (id type)) ; A Scheme Import Expression  
  (define-struct (if-exp term) (guard then else)) ; Cannot be made into an ms because it uses lazy evaluation
  (define-struct (litnum term) (val))
  (define-struct (litstr term) (val))
  (define-struct (bool term) (val))
  (define-struct (tuple term) (members))
  (define-struct (unit-term term) ())
  (define-struct (prim term) (name))
  (define-struct (constr-decl/0ary term) (name))
  (define-struct (constr-decl term) (name type))
  (define-struct (constr term) (name))
  (define-struct (typedef term) (name variants)) ;; variants is (listof (union constr-decl? constr-decl/0ary?))
  
  ; In order to add new primitive types, modify the following functions: type-equal? tyrep contains-tyvar? get-unbound-tyvars type create-export-contract create-import-contract create-import-wrapper emit-code
  ;; why does the type struct have a "source" field?
  (define-struct type (source) (make-inspector))
  (define-struct (tyvar type) (val) (make-inspector))
  (define-struct (arrow-type type) (param-type result-type) (make-inspector))  
  (define-struct (int-type type) () (make-inspector))
  (define-struct (str-type type) () (make-inspector))
  (define-struct (bool-type type)() (make-inspector))
  (define-struct (lump-type type) () (make-inspector))
  (define-struct (forall-type type) (tyvar partial-type) (make-inspector))
  (define-struct (tuple-type type) (member-types) (make-inspector))
  (define-struct (unit-type type) () (make-inspector))
  (define-struct (user-type type) (name) (make-inspector))
  
  (define emit-code 
    (case-lambda 
      ((term) (emit-code term #f))
      ((term type) ; The type is only used for let-defs
       (letrec ([emit-curried-lambda
                 (lambda (params body)
                   (if (null? params) 
                       (emit-code body)
                       (let ([new-id (fresh-id)])
                         #`(lambda (#,new-id) #,(emit-tuple-bindings new-id (car params) (emit-curried-lambda (cdr params) body))))))])   
         (match term
           ;; typedefs generate no code:
           [($ typedef _ _ _)
            #`(void)]
           [($ ms stx id type) 
            #`(#,(emit-import-check type) #,(emit-code id))]
           [($ let-def stx id params body rec)
            (match type
              [($ int-type source) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]
              [($ str-type source) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]
              [($ bool-type source) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]              
              [($ unit-type source) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]                            
              [($ lump-type source) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]        
              [($ arrow-type source param-type result-type)
               #`(define #,(datum->syntax-object #f (id-val id))
                   (contract #,(create-export-contract type) #,(emit-curried-lambda params body) 'ml 'scheme))]
              [($ forall-type source forall-tyvar partial-type) 
               (emit-code term partial-type)]
              [($ tuple-type source member-types) #`(define #,(datum->syntax-object #f (id-val id)) #,(emit-code body))]        
              [($ tyvar source val) (error 'emit-code "Expression ~v has type ~v.\n" term val)] 
              [_ (error 'emit-code "Invalid type: ~v." type)])]
           [($ let-exp stx id params exp body rec) 
            (if rec
                #`(letrec ((#,(datum->syntax-object #f (id-val id)) #,(emit-curried-lambda params exp))) #,(emit-code body))
                #`(let ((#,(datum->syntax-object #f (id-val id)) #,(emit-curried-lambda params exp))) #,(emit-code body)))]
           [($ anonymous-function stx params body) (emit-curried-lambda params body)]
           ;; special-case application of constructors:
           [($ app _ ($ constr _ name) args)
            (unless (and (pair? args) (null? (cdr args)))
              (error 'emit-code "internal should-be-impossible: constructor used in curried application"))
            #`(vector-immutable name (emit-code (car args)))]
           [($ app stx fun args)
            
            (letrec ([emit-curried-app
                      (lambda (fun args)
                        (if (null? args)
                            fun
                            (emit-curried-app `(,fun ,(car args)) (cdr args))))])
              (emit-curried-app (emit-code fun) (map emit-code args)))]
           [($ if-exp stx guard then else) 
            #`(if #,(emit-code guard) #,(emit-code then) #,(emit-code else))]       
           [($ id stx val) (let ((v (datum->syntax-object #f val)))
                             #`(if (promise? #,v) (force #,v) #,v))]
           [($ tuple stx members) #`(vector-immutable #,@(map emit-code members))]
           [($ litnum stx val) (datum->syntax-object #f val)] 
           [($ litstr stx val) (datum->syntax-object #f val)]
           [($ bool stx val) (datum->syntax-object #f val)]
           [($ unit-term stx) (datum->syntax-object #f (vector-immutable))]
           [($ prim stx name) (prim-table-lookup name)]
           ;; zero-ary constructors:
           [($ constr _ name) #`(vector-immutable name)])))))
  
  
  (define (prim-table-lookup name)
    (case name
      [(equals) #`(lambda (x)
                    (lambda (y)
                      (cond 
                        [(procedure? x) (raise "Invalid_argument \"equal: functional value\"")]
                        [(integer? x) (= x y)]
                        [else (equal? x y)])))]))
  
  
  
  
  ; Bind a set of params structs, which may contain tuples to the parameters of the actual lambda
  (define (emit-tuple-bindings var param item)
    (letrec ([bind-params-to-var 
              (lambda (var params item index)
                (if (null? params)
                    item
                    (bind-params-to-var var (cdr params) (bind-param-to-var var (car params) item index) (if index (+ index 1) #f))))]
             [bind-param-to-var 
              (lambda (var param item index)
                (match param
                  [($ id stx val) (let ((i (fresh-id)))
                                    #`(let* ((#,i #,(if index #`(vector-ref #,var #,index) var))
                                             (#,val (if (promise? #,i) (force #,i) #,i)))
                                        #,item))]
                  [($ tuple stx members) (let ([new-var (fresh-id)]
                                               (i (fresh-id)))
                                           #`(let* ((#,i #,(if index #`(vector-ref #,var #,index) var))
                                                    (#,new-var (if (promise? #,i) (force #,i) #,i)))
                                               #,(bind-params-to-var new-var members item 0)))]))])
      (bind-param-to-var var param item #f)))
  
  ; Emit a function to do an import check. It will return a value if it passes or raise an error otherwise.
  ; Return the code to do a first order check or to attach a contract to the value.
  ; We cannot assume that item is an id. It is just some amount of code that will mean something once in Scheme.
  (define (emit-import-check type)
    (define (make-err source item-stx)
      (if source
          (quasisyntax/loc source
            (error (format #,(string-append "Expected something of type " (type->string type) ", got: ~v") #,item-stx)))
          (quasisyntax
           (error (format #,(string-append "Expected something of type " (type->string type) ", got: ~v") #,item-stx)))))
    (match type
      [($ int-type source) 
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             (if (integer? item) item #,(make-err source #`item))))]
      [($ str-type source)
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             (if (string? item) item #,(make-err source #`item))))]
      [($ bool-type source)
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             (if (boolean? item) item #,(make-err source #`item))))]
      [($ unit-type source)
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             (if (and (vector? item) (= (vector-length item) 0)) (vector-immutable) #,(make-err source #`item))))]
      [($ lump-type source)
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             item))]
      [($ arrow-type source param-type result-type)
       #`(lambda (item)
           (let ((item (if (promise? item) (force item) item)))
             (if (procedure? item) 
                 #,(create-import-wrapper #`(contract #,(create-import-contract type) item 'import-from-scheme 'ml) type)
                 #,(make-err source #`item))))]
      [($ forall-type source forall-tyvar partial-type) (emit-import-check partial-type)] 
      [($ tuple-type source member-types) 
       (let ([member-checks (map emit-import-check member-types)])
         #`(lambda (item)
             (let ((item (if (promise? item) (force item) item)))
               (if (and (vector? item) (= (vector-length item) #,(length member-types)))
                   ; The scheme value does not have to be an immutable vector, but it will be converted to one
                   (vector->immutable-vector (list->vector 
                                              (map (lambda (member-check member) (member-check member)) (list #,@member-checks) (vector->list item))))
                   #,(make-err source #`item)))))]
      ;; RIGHT HERE: well, we have to add a type environment everywhere...
      [_ (error 'emit-code "Invalid type: ~v." type)]))
  
  (define (emit-wrap-code tyvar)
    (string->symbol (string-append "_wrap_" (symbol->string tyvar))))
  (define (emit-unwrap-code tyvar)
    (string->symbol (string-append "_unwrap_" (symbol->string tyvar))))
  (define (emit-wrapped-code tyvar)
    (string->symbol (string-append "_" (symbol->string tyvar) "?")))
  
  ; Do lump embedding for tyvars
  (define (create-export-contract type)
    (match type
      [($ int-type source) 'integer?]
      [($ bool-type source) 'boolean?]
      [($ str-type source) 'string?]
      [($ unit-type source) #`(vector-immutable/c)]      
      [($ lump-type source) 'any/c]        
      [($ arrow-type source param-type result-type)
       #`(-> #,(create-export-contract param-type) #,(create-export-contract result-type))]
      [($ tyvar source val) 'any/c]
      [($ forall-type source forall-tyvar partial-type) (create-export-contract partial-type)]
      ; Have to use vector-immutable/c, otherwise it can't have function contracts.
      [($ tuple-type source member-types) #`(vector-immutable/c #,@(map create-export-contract member-types))]
      [_ (error 'create-export-contract "Invalid type: ~v." type)])) 
  
  ; Here, all tyvars are part of a forall type. These must be wrapped in a struct in order to preserve parametricity.
  (define (create-import-contract type) 
    (match type
      [($ int-type source) 'integer?]
      [($ str-type source) 'string?]
      [($ bool-type source) 'boolean?]
      [($ lump-type source) 'any/c]  
      [($ unit-type source) #`(vector/c)]
      [($ arrow-type source param-type result-type)
       #`(-> 
          #,(create-import-contract param-type) #,(create-import-contract result-type))] 
      [($ tyvar source val) (emit-wrapped-code val)]
      [($ forall-type source forall-tyvar partial-type) (create-import-contract partial-type)]
      ; Have to use vector-immutable/c, otherwise it can't have function contracts.      
      [($ tuple-type source member-types) #`(vector-immutable/c #,@(map create-export-contract member-types))]
      [_ (error 'create-import-contract "Invalid type: ~v." type)]))
  
  
  (define (create-import-wrapper import type)
    (letrec ([create-import-wrapper-internal
              (lambda (import type pos tyvars)
                (match type
                  [($ int-type source) import]
                  [($ str-type source) import]
                  [($ bool-type source) import]
                  [($ unit-type source) import]                  
                  [($ lump-type source) import]        
                  [($ tuple-type source member-types) import] 
                  [($ arrow-type source param-type result-type) 
                   (let ([var (fresh-id)])
                     (letrec ([param (create-import-wrapper-internal var param-type (not pos) null)]
                              [result (create-import-wrapper-internal #`(#,import #,param) result-type pos tyvars)])
                       (if (or (forall-type? param-type) (forall-type? result-type))
                           (error 'create-import-wrapper "Unexpected nested forall type"))
                       (if (or (null? tyvars) (arrow-type? result-type))
                           #`(lambda (#,var) #,result)
                           #`(lambda (#,var)
                               (let-values (#,@(map 
                                                (lambda (tyvar) `((,(emit-wrap-code tyvar) ,(emit-unwrap-code tyvar) ,(emit-wrapped-code tyvar))
                                                                  ,(datum->syntax-object #f '(new-wrapper) #f #f)))
                                                tyvars))
                                 #,result)))))]
                  [($ tyvar source val) 
                   (if pos
                       #`(#,(emit-unwrap-code val) #,import)
                       #`(#,(emit-wrap-code val) #,import))]
                  [_ (error 'create-import-wrapper "Invalid type: ~v." type)]))])
      (create-import-wrapper-internal import type #t (map tyvar-val (get-unbound-tyvars type)))))
  
  (define (type->string type)
    (match type
      [($ int-type source) "int"]
      [($ str-type source) "string"]
      [($ lump-type source) "lump"]        
      [($ bool-type source) "bool"]        
      [($ unit-type source) "()"]            
      [($ arrow-type source param-type result-type)
       (string-append (type->string param-type) "->" (type->string result-type))]
      [($ tyvar source val) (format "~a" val)]
      [($ forall-type source forall-tyvar partial-type) 
       (string-append "(forall " (type->string forall-tyvar) " " (type->string partial-type) ")")]
      [($ tuple-type stx members) 
       (letrec ([tuple->string (lambda (members str) 
                                 (if (null? members) str (tuple->string (cdr members) (string-append str " * " (type->string (car members))))))])
         (string-append "(" (tuple->string (cdr members) (type->string (car members))) ")"))]
      [_ (error 'type->string "Invalid type: ~v." type)]))
  
  ;; generate fresh ids
  (define fresh-id-index 0)
  (define (fresh-id)
    (set! fresh-id-index (+ fresh-id-index 1))
    (string->symbol (format "_id_~v" fresh-id-index)))
  
  (define ((type-neq? a) b) (not (type-equal? a b)))
  
  ; Get the tyvars out of a type with an option to include bound tyvars or not
  (define (get-tyvars type include-bound-tyvars)
    (define (maybe-filter bound-tyvar tyvars)
      (if include-bound-tyvars
          tyvars
          (filter (type-neq? bound-tyvar) tyvars)))
    (match type
      [($ int-type _) '()]
      [($ str-type _) '()]
      [($ lump-type _) '()]      
      [($ bool-type _) '()]
      [($ unit-type _) '()]      
      [($ tyvar _ val) (list type)] 
      [($ arrow-type _ param-type result-type)
       (tyvar-union (list (get-tyvars param-type include-bound-tyvars)
                          (get-tyvars result-type include-bound-tyvars)))]
      [($ forall-type _ forall-tyvar partial-type)
       (maybe-filter forall-tyvar (get-tyvars partial-type include-bound-tyvars))]      
      [($ tuple-type source member-types) 
       (tyvar-union (map (lambda (member-type) (get-tyvars member-type include-bound-tyvars)) member-types))]
      [_ (error 'get-tyvars "Invalid type: ~v." type)]))  
  
  (define (get-unbound-tyvars type) (get-tyvars type #f))
  
  
  (define (get-all-tyvars type) (get-tyvars type #t))
  
  ; Take the union of a list of lists and remove all duplicates.
  ; get-uid is a function that gets a unique identifier for the items in the list
  (define (list-union lists get-uid)
    (foldl (lambda (new-list acc)
             (append (filter 
                      (lambda (item) (not (member (get-uid item) (map get-uid acc))))
                      new-list) acc))
           null
           lists))
  
  (define (tyvar-union lists)
    (list-union lists tyvar-val))
  
  (define (id-union lists)
    (list-union lists id-val))
  
  
  
  ; Check for alpha equivalence of two types
  (define (type-equal? type1 type2)
    (match (list type1 type2)
      [(($ int-type _) ($ int-type _)) #t]
      [(($ str-type _) ($ str-type _)) #t]
      [(($ lump-type _) ($ lump-type _)) #t]
      [(($ bool-type _) ($ bool-type _)) #t]
      [(($ unit-type _) ($ unit-type _)) #t]      
      [(($ tyvar _ val1) ($ tyvar _ val2)) (eq? val1 val2)]
      [(($ arrow-type _ from-type1 to-type1) ($ arrow-type _ from-type2 to-type2))
       (and (type-equal? from-type1 from-type2) (type-equal? to-type1 to-type2))]
      ;; this could be needlessly slow... :
      [(($ forall-type _ tyvar1 body-type1) ($ forall-type _ tyvar2 body-type2))
       (type-equal? body-type1 (tyrep body-type2 tyvar2 tyvar1))]
      [(($ tuple-type _ member-types1) ($ tuple-type _ member-types2))
       (and (= (length member-types1) (length member-types2)) (andmap type-equal? member-types1 member-types2))]
      [_ #f]))
  
  ; Return a new type based on source-type with all instances of old-type replaced with new-type
  (define (tyrep source-type old-type new-type)
    (if (tyvar? old-type)
        (begin
          (match source-type
            [($ int-type source) source-type]
            [($ str-type source) source-type]
            [($ lump-type source) source-type]    
            [($ bool-type source) source-type]
            [($ unit-type source) source-type]            
            [($ tyvar source val)
             (if (type-equal? source-type old-type) new-type source-type)]      
            [($ arrow-type source param-type result-type)
             (make-arrow-type source (tyrep param-type old-type new-type) (tyrep result-type old-type new-type))]
            [($ forall-type source forall-tyvar partial-type) 
             (if (type-equal? old-type forall-tyvar)
                 source-type
                 (make-forall-type source forall-tyvar (tyrep partial-type old-type new-type)))]
            [($ tuple-type source member-types)
             (make-tuple-type source (map (lambda (type) (tyrep type old-type new-type)) member-types))]
            [_ (error 'tyrep "Invalid type: ~v." source-type)]))
        (error 'tyrep "Can only replace tyvars, got ~v" old-type)))
  
  
  )