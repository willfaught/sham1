(module list mzscheme
  (require (lib "match.ss"))
  
  (provide (all-defined))
  
  ; drop :: [a] -> integer -> [a]
  (define (drop x n)
    (if (not (list? x))
        (error 'drop "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'drop "invalid list length"))
    (if (equal? n 0)
        x
        (drop (cdr x) (- n 1))))
  
  ; take :: [a] -> integer -> [a]
  (define (take x n)
    (if (not (list? x))
        (error 'take "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'take "invalid list length"))
    (if (equal? n 0)
        null
        (cons (car x) (take (cdr x) (- n 1)))))
  
  ; foldr1 :: (a -> a -> a) -> [a] -> a
  (define (foldr1 f xs)
    (match xs
      ((x) x)
      ((x . xs) (f x (foldr1 f xs)))
      (() (error 'foldr1 "empty list")))))