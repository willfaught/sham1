(module List scheme
  (require (lib "Maybe.ss" "sham" "haskell"))
  
  (provide (all-defined-out))
  
  (define (foldl1 f xs)
    (match xs
      ((cons x xs) (foldl f x xs))
      (null (error 'foldl1 "empty list"))))
  
  (define (foldr1 f xs)
    (match xs
      ((list x) x)
      ((cons x xs) (f x (foldr1 f xs)))
      (null (error 'foldr1 "empty list"))))
  
  (define (iterate f x n)
    (if (equal? n 0) null (cons x (iterate f (f x) (- n 1)))))
  
  (define (lookup name pairs)
    (match pairs
      ((cons (list pairName pairValue) xs) (if (equal? name pairName) (make-Just pairValue) (lookup name xs)))
      (null (make-Nothing))))
  
  (define (zip xs ys)
    (match (list xs ys)
      ((list (cons x xs) (cons y ys)) (cons (list x y) (zip xs ys)))
      ((list null null) null)
      ((list xs (list)) (error 'zip "first list is longer"))
      ((list (list) ys) (error 'zip "second list is longer"))))
  
  (define (zipWith f xs ys)
    (match (list xs ys)
      ((list (cons x xs) (cons y ys)) (cons (f x y) (zipWith f xs ys)))
      ((list null null) null)
      ((list xs (list)) (error 'zipWith "first list is longer"))
      ((list (list) ys) (error 'zipWith "second list is longer")))))