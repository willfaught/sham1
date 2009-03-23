(module ml-typechecker mzscheme

  (require (lib "match.ss")
           (lib "list.ss")
           "ml-expander.ss"
           "stack.ss"
           (only (lib "list.ss" "srfi" "1") pair-fold))
  
  (provide (all-defined))
  
  
   
  (define-struct constraint (first-type second-type) (make-inspector))
  

 
  ;;
  ;; TYPECHECK
  ;;
  
  ;; JBC: this code is very grody (sorry, David).  In particular, AFAICT there's absolutely no
  ;; need for the stateful stack.
  
  ; Check the type of the expression and make sure that all the variables are bound.
  ; The css is the constraint stack stack, a stack of stacks of constraints.
  ; There is one stack of constraints for each nested let expression.
  (define ((typecheck usertypes) term typemap css)
    (let ([recur (typecheck usertypes)]
          [constraint-stack (stack-peek css)])
      (match term
        [($ let-def stx id params body rec) 
         (letrec 
             ([set-var-type
               (lambda (var var-type)
                 (if (not var-type)
                     (hash-table-remove! typemap (id-val var))
                     (hash-table-put! typemap (id-val var) var-type)))] 
              [get-fun-type 
               (lambda (param-types result-type)
                 (if (null? param-types)
                     result-type
                     (make-arrow-type term (car param-types) (get-fun-type (cdr param-types) result-type))))]
              [replace-in-typemap
               (lambda (tyvar new-type)
                 (set! tyvar (make-tyvar tyvar tyvar))
                 (hash-table-for-each typemap
                                      (lambda (var type)
                                        (if (contains-tyvar? type tyvar)
                                            (hash-table-put! typemap var (tyrep type tyvar new-type))))))]
              [fresh-param-type 
               (lambda (param)
                 (if (tuple? param) 
                     (make-tuple-type param (map fresh-param-type (tuple-members param)))
                     (make-tyvar param (fresh-tyvar))))])
           (let ([param-types (map fresh-param-type params)]
                 [prev-var-types (map (lambda (param) (hash-table-get typemap (id-val param) (lambda () #f))) (flatten-params params))]
                 [constraint-stack (new-stack)])
             (stack-push! css constraint-stack)
             (if (hash-table-get typemap (id-val id) (lambda () #f)) 
                 (error 'typecheck "Redefinition of ~v." (id-val id))
                 (if rec (hash-table-put! typemap (id-val id) (make-tyvar id (fresh-tyvar)))))
             (let ([external-tyvars (tyvar-union (hash-table-map typemap (lambda (var type) (get-all-tyvars type))))])
               (for-each set-var-type (flatten-params params) (flatten-param-types param-types))
               (let ([result-type (recur body typemap css)])
                 (let ([fun-type (get-fun-type param-types result-type)])
                   (if rec
                       (stack-push! constraint-stack 
                                    (make-constraint fun-type (hash-table-get typemap (id-val id) 
                                                                              (lambda () (error 'typecheck "Unbound variable: ~v." (id-val id))))))
                       (hash-table-put! typemap (id-val id) fun-type))
                   (let ([replacements (unify constraint-stack (make-hash-table))])
                     (hash-table-for-each replacements replace-in-typemap)                       
                     (set! external-tyvars (filter tyvar? (map (lambda (tyvar) 
                                                                 (hash-table-get replacements (tyvar-val tyvar) (lambda () tyvar))) external-tyvars)))
                     (set! fun-type (generalize (hash-table-get typemap (id-val id)) external-tyvars))
                     (hash-table-put! typemap (id-val id) fun-type)
                     (map set-var-type (flatten-params params) prev-var-types)
                     (stack-pop! css)
                     
                     #|(display (string-append (symbol->string (id-val id)) " has type " (type->string fun-type) ". ")) 
                     (display (format "Parameters: ~v. " (map param->list params)))
                     (display (format "Parameter Types: ~v. " (map type->string param-types)))
                     (display (format "Flat params: ~v. " (map id-val (map param->list (flatten-params params)))))
                     (display (format "Parameter Types: ~v." (map type->string (flatten-param-types param-types))))                     
                     (display (format "External Tyvars: ~v." (map type->string external-tyvars)))
                     (newline)                       |#
                     
                     fun-type))))))]
        [($ let-exp stx id params exp body rec) 
         (let ([prev-id-type (hash-table-get typemap (id-val id) (lambda () #f))])
           (if prev-id-type (hash-table-remove! typemap (id-val id)))
           (recur (make-let-def (term-stx exp) id params exp rec) typemap css)
           (let ([body-type (recur body typemap css)])
             (if (eq? prev-id-type #f) 
                 (hash-table-remove! typemap (id-val id))
                 (hash-table-put! typemap (id-val id) prev-id-type))
             body-type))]
        [($ anonymous-function stx params body) 
           (recur (make-let-def (term-stx body) (make-id #f (fresh-id)) params body #f) typemap css)]
        [($ app stx fun args)
         (letrec ([typecheck-app
                   (lambda (fun-type arg-types)
                     (if (null? arg-types)
                         fun-type
                         (let ([result-type (make-tyvar term (fresh-tyvar))])
                           (stack-push! constraint-stack (make-constraint fun-type (make-arrow-type term (car arg-types) result-type)))
                           (typecheck-app result-type (cdr arg-types)))))])
           (typecheck-app (recur fun typemap css) (map (lambda (term) (recur term typemap css)) args)))]
        [($ if-exp stx guard then else) 
         (let ([guard-type (recur guard typemap css)] 
               [type1 (recur then typemap css)] 
               [type2 (recur else typemap css)])
           (stack-push! constraint-stack (make-constraint guard-type (make-bool-type guard)))
           (stack-push! constraint-stack (make-constraint type1 type2))
           type1)]   
        [($ tuple stx members)
         (make-tuple-type #f (map (lambda (member) (recur member typemap css)) members))]
        [($ id stx val) 
         (hash-table-get typemap val
                         (lambda () (error 'typecheck "Unbound variable: ~v." val)))]
        [($ litnum stx val) (make-int-type term)]
        [($ litstr stx val) (make-str-type term)]
        [($ bool stx val) (make-bool-type term)]
        [($ unit-term stx) (make-unit-type term)]
        [($ prim stx name) (lookup-prim-type name)]
        #;[($ constr _ name) (find-constr-type usertypes name)]
        [($ ms stx id type) type])))
  
  ;; collision check : make sure no user-defined data types can collide
  (define (collision-check usertypes)
    (no-dups? "type" (append `("int" "bool" "string") (map typedef-name usertypes)))
    (no-dups? "constructor" (map constr-decl-name/either (apply append (map typedef-variants usertypes)))))
  
  (define (constr-decl-name/either constr)
    (match constr
      [($ constr-decl _ name _) name]
      [($ constr-decl/0ary _ name) name]))
  
  (define (no-dups? kindstr l)
    (let loop ([l l])
      (if (null? l)
          (void)
          (if (member (car l) (cdr l))
              (error 'collision-check "duplicated ~a name: ~v" kindstr (car l))
              (loop (cdr l))))))
  
  (define (lookup-prim-type name)
    (case name
      [(equals) (anon-arrow (anon-tyvar 'a) (anon-tyvar 'a) anon-bool)]
      [else (error 'lookup-prim-type "internal error: reference to unknown prim: ~v" name)]))
  
  
  ; Helper functions to create anonymous types and an anonymous ms
  ; These are used during parsing when operations and if statements are changed to scheme imports
  ;; this function maps, e.g., (int bool int unit) to (int -> (bool -> (int -> unit)))
  (define (anon-arrow . types)
    (if (null? types)
        (error 'new-arrow "type list is empty")
        (if (null? (cdr types))
            (car types)
            (make-arrow-type #f (car types) (apply anon-arrow (cdr types))))))
  
  (define anon-int (make-int-type #f))
  (define anon-bool (make-bool-type #f))
  (define anon-lump (make-lump-type #f))
  (define (anon-tyvar token) (make-tyvar #f token))
  (define (anon-forall token type) (make-forall-type #f (anon-tyvar token) type))
  (define anon-unit-type (make-unit-type #f))
  
  (define anon-unit-term (make-unit-term #f))
    
  (define (anon-ms id type) (make-ms #f id (replace-unbound-tyvars-with-fresh-tyvars type)))
  (define (anon-id id) (make-id #f id))

    
  ; Flatten params
  ; To test this, use (map id-val (flatten-params (list (make-tuple #f (list (make-tuple #f (list (anon-id 'a) (anon-id 'b))) (anon-id 'c))) (anon-id 'd)))). It should evaluate to (a b c d)
  (define (flatten-params params)
    (letrec ([get-ids-from-param 
              (lambda (param)
                (match param 
                  [($ id stx val) (list param)]
                  [($ tuple stx members) (flatten-lists (map get-ids-from-param members))]
                  [else (error 'get-ids-from-param "Invalid param: ~v" param)]))])
      (flatten-lists (map get-ids-from-param params))))
  
  ; Flatten params
  (define (flatten-param-types param-types)
    (letrec ([helper
              (lambda (param-type)
                (match param-type 
                  [($ tuple-type src member-types) (flatten-lists (map helper member-types))]
                  [else (list param-type)]))])
      (flatten-lists (map helper param-types))))
  
    ; Generalize all tyvars in a type except for the list of external-tyvars passed in
  (define (generalize type external-tyvars)
    (wrap-forall-types-around type 
                              (filter (lambda (tyvar) (not (member (tyvar-val tyvar) (map tyvar-val external-tyvars))))
                                      (get-unbound-tyvars type))))    
  
  ;; maps  T (a b c)  --->   forall c . forall b. forall a . T
  (define (wrap-forall-types-around type tyvars)
    (foldl wrap-with-tyvar type tyvars))
  
  ;; wrap-with-tyvar tyvar t = forall t.source tyvar t
  (define (wrap-with-tyvar tyvar t) (make-forall-type (type-source t) tyvar t))
  
  (define (replace-unbound-tyvars-with-fresh-tyvars type)
    (foldl replace-one-tyvar type (get-unbound-tyvars type)))
  
  (define (replace-one-tyvar tyvar type)
    (tyrep type tyvar (make-tyvar (type-source tyvar) (fresh-tyvar))))
  
  
  ; replacements - a hash table whose keys specify a tyvar to be replaced, and whose values represent what to replace the tyvar with
  (define (unify constraint-stack replacements)
    (if (stack-empty? constraint-stack)
        replacements
        (let ([constraint-replace 
               (lambda (tyvar new-type) 
                 (lambda (constraint)
                   (let ([type1 (constraint-first-type constraint)] [type2 (constraint-second-type constraint)])
                     (make-constraint (tyrep type1 tyvar new-type) (tyrep type2 tyvar new-type)))))]
              [replacement-replace
               (lambda (tyvar1 type1)
                 (lambda (tyvar2 type2)
                   (set! tyvar2 (make-tyvar tyvar2 tyvar2))
                   (if (contains-tyvar? type2 tyvar1)
                       (hash-table-put! replacements (tyvar-val tyvar2) (tyrep type2 tyvar1 type1)))
                   (if (contains-tyvar? type1 tyvar2)
                       (hash-table-put! replacements (tyvar-val tyvar1) (tyrep type1 tyvar1 type2)))))])
          (let ([constraint (stack-pop! constraint-stack)])
            (let ([type1 (constraint-first-type constraint)] [type2 (constraint-second-type constraint)])
              (cond [(type-equal? type1 type2) void]
                    [(and (tyvar? type1) (not (contains-tyvar? type2 type1))) 
                     (if (hash-table-get replacements (tyvar-val type1) (lambda () #f))
                         (stack-push! constraint-stack (make-constraint type2 (hash-table-get replacements (tyvar-val type1))))
                         (begin
                           (hash-table-put! replacements (tyvar-val type1) type2)
                           (hash-table-for-each replacements (replacement-replace type1 type2))
                           (set! type2 (hash-table-get replacements (tyvar-val type1)))
                           (set-stack-data! constraint-stack (map (constraint-replace type1 type2) (stack-data constraint-stack)))))]
                    [(and (tyvar? type2) (not (contains-tyvar? type1 type2))) 
                     (if (hash-table-get replacements (tyvar-val type2) (lambda () #f))
                         (stack-push! constraint-stack (make-constraint type1 (hash-table-get replacements (tyvar-val type2))))
                         (begin 
                           (hash-table-put! replacements (tyvar-val type2) type1)                     
                           (hash-table-for-each replacements (replacement-replace type2 type1))
                           (set! type1 (hash-table-get replacements (tyvar-val type2)))                     
                           (set-stack-data! constraint-stack (map (constraint-replace type2 type1) (stack-data constraint-stack)))))] 
                    [(and (arrow-type? type1) (arrow-type? type2))
                     (match type1 [($ arrow-type source1 param-type1 result-type1) 
                                   (match type2 [($ arrow-type source2 param-type2 result-type2) 
                                                 (stack-push! constraint-stack (make-constraint param-type1 param-type2))
                                                 (stack-push! constraint-stack (make-constraint result-type1 result-type2))])])]
                    [(forall-type? type1)
                     (match type1 [($ forall-type source tyvar partial-type) 
                                    (let ([new-type (tyrep partial-type tyvar (make-tyvar source (fresh-tyvar)))])
                                      (stack-push! constraint-stack (make-constraint new-type type2)))])]
                    [(forall-type? type2)
                     (match type2 [($ forall-type source tyvar partial-type) 
                                    (let ([new-type (tyrep partial-type tyvar (make-tyvar source (fresh-tyvar)))])
                                      (stack-push! constraint-stack (make-constraint type1 new-type)))])]                                    
                    [(and (tuple-type? type1) (tuple-type? type2))
                     (match type1 [($ tuple-type source1 member-types1) 
                                   (match type2 [($ tuple-type source2 member-types2)
                                                 (if (= (length member-types1) (length member-types2))
                                                     (for-each 
                                                      (lambda (t1 t2) (stack-push! constraint-stack (make-constraint t1 t2))) 
                                                      member-types1 member-types2)
                                                     (error 'unify (string-append "Incompatible types: " (type->string type1) " and " (type->string type2) ".")))])])] 
                    [else (error 'unify (string-append "Incompatible types: " (type->string type1) " and " (type->string type2) "."))])
              (unify constraint-stack replacements))))))
  
   
  
  ; Take a list of lists and flatten them
  (define (flatten-lists lists)
    (letrec ([helper 
              (lambda (lists acc)
                (if (null? lists)
                    acc
                    (append (car lists) (helper (cdr lists) acc))))])
    (helper lists '())))
  

  
  (define (contains-tyvar? type target-tyvar)
    (if (tyvar? target-tyvar)
        (match type
          [($ int-type source) #f]
          [($ str-type source) #f]
          [($ lump-type source) #f]  
          [($ bool-type source) #f]
          [($ unit-type source) #f]          
          [($ tyvar source val) 
           (eq? val (tyvar-val target-tyvar))]
          [($ arrow-type source param-type result-type)
           (or (contains-tyvar? param-type target-tyvar) (contains-tyvar? result-type target-tyvar))]
          [($ forall-type source forall-tyvar partial-type)
           (and (not (type-equal? forall-tyvar target-tyvar)) (contains-tyvar? partial-type target-tyvar))]  
          [($ tuple-type source member-types)
           (ormap (lambda (type2) (contains-tyvar? type2 target-tyvar)) member-types)]
          [_ (error 'contains-tyvar? "Invalid type: ~v." type)])
        (error 'contains-tyvar? "Invalid tyvar: ~v." target-tyvar)))
  
  
  
  (define fresh-tyvar-index 0)
  (define (fresh-tyvar)
    (set! fresh-tyvar-index (+ fresh-tyvar-index 1))
    (string->symbol (format "_tyvar_~v" fresh-tyvar-index))))