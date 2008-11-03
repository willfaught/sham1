(module List mzscheme
  (require (only (lib "list.ss") foldl)
           (only (lib "match.ss") match))
  
  (provide (all-defined))
  
  ; drop :: [a] integer -> [a]
  (define (drop x n)
    (if (not (list? x))
        (error 'drop "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'drop "invalid list length"))
    (if (equal? n 0)
        x
        (drop (cdr x) (- n 1))))
  
  ; foldl1 :: (a -> a -> a) [a] -> a
  (define (foldl1 f xs)
    (match xs
      ((x . xs) (foldl f x xs))
      (() (error 'foldl1 "empty list"))))
  
  ; foldr1 :: (a -> a -> a) [a] -> a
  (define (foldr1 f xs)
    (match xs
      ((x) x)
      ((x . xs) (f x (foldr1 f xs)))
      (() (error 'foldr1 "empty list"))))
  
  ; iterate :: (a -> a) a integer -> [a]
  (define (iterate f x n)
    (if (equal? n 0) null (cons x (iterate f (f x) (- n 1)))))
  
  ; take :: [a] integer -> [a]
  (define (take x n)
    (if (not (list? x))
        (error 'take "not a list"))
    (if (or (< n 0) (> n (length x)))
        (error 'take "invalid list length"))
    (if (equal? n 0)
        null
        (cons (car x) (take (cdr x) (- n 1)))))
  
  ; zipWith :: (a -> b -> c) [a] [b] -> [c]
  (define (zipWith f x y)
    (if (equal? (length x) (length y))
        (if (null? x) null (cons (f (car x) (car y)) (zipWith f (cdr x) (cdr y))))
        (error 'zipWith "lists have different lengths"))))