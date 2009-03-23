(module stack mzscheme

  (provide (all-defined))
  
  (define-struct stack (data))
  
  (define (new-stack)
    (make-stack '())
  )
  
  (define stack->list stack-data)
  (define (list->stack l) (make-stack l))
  
  (define (stack-empty? stack)
    (null? (stack-data stack))
  )
  
  (define (stack-push! stack item)
    (set-stack-data! stack (cons item (stack-data stack)))
  )
  
  (define (stack-pop! stack)
    (if (null? (stack-data stack))
        (error 'stack-pop! "Trying to pop an empty stack.")
        (let ([first (car (stack-data stack))] [rest (cdr (stack-data stack))])
          (set-stack-data! stack rest)
          first
        )
    )
  )
  
  (define (stack-peek stack)
    (if (null? (stack-data stack))
        (error 'stack-peek! "Trying to peek an empty stack.")
        (car (stack-data stack))
    )
  )
)