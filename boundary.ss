(module boundary mzscheme
  (require (only (lib "1.ss" "srfi") list-tabulate zip)
           (lib "match.ss")
           (lib "types.ss" "sham" "haskell"))
  
  (provide haskell->ml haskell->scheme ml->haskell scheme->haskell)
  
  ; haskell->ml :: type term integer -> datum
  (define (haskell->ml type term depth)
    (match type
      (($ character-type) (error 'haskell->ml "ML does not support a character type"))
      (($ float-type) term)
      (($ function-type p r) (let ((i (identifier depth)))
                               `(lambda (,i) ,(haskell->ml r `(,term (delay ,(ml->haskell p i (+ depth 1)))) (+ depth 1)))))
      (($ integer-type) term)
      (($ list-type _) (error 'haskell->ml "ML does not support a compound type"))
      (($ tuple-type t) `(vector-immutable ,@(map (match-lambda ((t i) (haskell->ml t `(vector-ref (force ,term) ,i) 1)))
                                                  (zip t (list-tabulate (length t) (lambda (x) x))))))
      (($ type-constructor "Bool") `(equal? (force ,term) (force haskell:True)))
      (($ type-constructor "()") `(begin (force ,term)
                                         (vector-immutable)))
      (($ type-constructor _) (error 'haskell->ml "ML does not support a compound type"))
      (($ type-variable _) term)))
  
  ; haskell->scheme :: type term integer -> datum
  (define (haskell->scheme type term depth)
    (let ((id `(lambda (x) x)))
      (match type
        (($ character-type) term)
        (($ float-type) term)
        (($ function-type p r) (let ((i (identifier depth)))
                                 `(lambda (,i) ,(haskell->scheme r `(,term (delay ,(scheme->haskell p i (+ depth 1)))) (+ depth 1)))))
        (($ integer-type) term)
        (($ list-type _) term)
        (($ tuple-type _) term)
        (($ type-constructor _) term)
        (($ type-variable _) `(make-lump ,term)))))
  
  ; identifier :: integer -> symbol
  (define (identifier n)
    (string->symbol (string-append "x" (number->string n))))
  
  ; ml->haskell :: type term integer -> datum
  (define (ml->haskell type term depth)
    (match type
      (($ function-type p r) (let ((i (identifier depth)))
                               `(lambda (,i) ,(ml->haskell r `(,term ,(haskell->ml p i (+ depth 1))) (+ depth 1)))))
      (($ integer-type) term)
      (($ list-type ($ character-type)) `(foldr (lambda (x y) (cons-immutable (delay x) (delay y))) null (string->list ,term)))
      (($ tuple-type t) (let* ((pairs (zip t (list-tabulate (length t) (lambda (x) x))))
                               (elements (map (match-lambda ((t i) `(delay ,(ml->haskell t `(vector-ref x ,i) depth)))) pairs)))
                          `(let ((x ,term)) (vector-immutable ,@elements))))
      (($ type-constructor "Bool") `(if ,term (force haskell:True) (force haskell:False)))
      (($ type-constructor "()") `(force haskell:|()|))
      (($ type-variable _) term)))
  
  ; scheme->haskell :: type term integer -> datum
  (define (scheme->haskell type term depth)
    (let ((f `(if (promise? ,term) (force ,term) ,term)))
      (match type
        (($ character-type) f)
        (($ float-type) f)
        (($ function-type p r) (let ((i (identifier depth)))
                                 `(lambda (,i) ,(scheme->haskell r `(,term ,(haskell->scheme p i (+ depth 1))) (+ depth 1)))))
        (($ integer-type) f)
        (($ list-type type) `(foldr (lambda (x y) (cons-immutable (delay ,(scheme->haskell type `x depth)) (delay y))) null ,f))
        (($ tuple-type types) (let* ((pairs (zip types (list-tabulate (length types) (lambda (x) x))))
                                     (elements (map (match-lambda ((type index) `(delay ,(scheme->haskell type `(vector-ref x ,index) depth)))) pairs)))
                                `(let ((x ,f)) (vector-immutable ,@elements))))
        (($ type-constructor _) f)
        (($ type-variable _) `(let ((x ,f)) (if (lump? x) (force (lump-contents x)) x)))))))