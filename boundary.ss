(module boundary mzscheme
  (require (lib "compiler.ss" "haskell")
           (lib "contract.ss")
           (lib "list.ss" "haskell")
           (lib "terms.ss" "haskell")
           (lib "types.ss" "haskell"))
  
  (provide scheme-haskell-boundary)

  
  
  #;(define (thunk-arguments function parameter-number)
    (define (nest-arguments function argument-count)
      (if (equal? argument-count parameter-number)
    (define (nest-functions function nest-count)
      (if (equal? nest-count parameter-number)
          function
          '(lambda (,(string->symbol (string-append "p" (+ nest-count 1)))) 
    (if (equal? parameter-number 0)
        function
        '(lambda (,(string->symbol ))))))))))
  
  (define (expand-haskell-boundary term type)
    (match term
      (($ 
  
  ; scheme-haskell-boundary :: term -> type -> term
  (define (distribute-boundaries term type)
    (match term
      (($ haskell-term term type) 'TODO)
      (($ scheme-term term type) 'TODO)))
  
  ; distribute-scheme-boundary :: scheme-term -> type ->
  (define (distribute-scheme-boundary term type)
