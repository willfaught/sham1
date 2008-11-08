(module List mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "match.ss"))
  
  (provide/contract (drop (-> (listof any/c) integer? (listof any/c)))
                    (foldl1 (-> (-> any/c any/c any/c) (listof any/c) any/c))
                    (foldr1 (-> (-> any/c any/c any/c) (listof any/c) any/c))
                    (iterate (-> (-> any/c any/c) any/c integer? (listof any/c)))
                    (take (-> (listof any/c) integer? (listof any/c)))
                    (zipWith (-> (-> any/c any/c any/c) (listof any/c) (listof any/c) (listof any/c))))
  
  (define/contract drop (-> (listof any/c) integer? (listof any/c))
    (lambda (x n)
      (if (not (list? x))
          (error 'drop "not a list"))
      (if (or (< n 0) (> n (length x)))
          (error 'drop "invalid list length"))
      (if (equal? n 0)
          x
          (drop (cdr x) (- n 1)))))
  
  (define/contract foldl1 (-> (-> any/c any/c any/c) (listof any/c) any/c)
    (lambda (f xs)
      (match xs
        ((x . xs) (foldl f x xs))
        (() (error 'foldl1 "empty list")))))
  
  (define/contract foldr1 (-> (-> any/c any/c any/c) (listof any/c) any/c)
    (lambda (f xs)
      (match xs
        ((x) x)
        ((x . xs) (f x (foldr1 f xs)))
        (() (error 'foldr1 "empty list")))))
  
  (define/contract iterate (-> (-> any/c any/c) any/c integer? (listof any/c))
    (lambda (f x n)
      (if (equal? n 0) null (cons x (iterate f (f x) (- n 1))))))
  
  (define/contract take (-> (listof any/c) integer? (listof any/c))
    (lambda (x n)
      (if (not (list? x))
          (error 'take "not a list"))
      (if (or (< n 0) (> n (length x)))
          (error 'take "invalid list length"))
      (if (equal? n 0)
          null
          (cons (car x) (take (cdr x) (- n 1))))))
  
  (define/contract zipWith (-> (-> any/c any/c any/c) (listof any/c) (listof any/c) (listof any/c))
    (lambda (f x y)
      (if (equal? (length x) (length y))
          (if (null? x) null (cons (f (car x) (car y)) (zipWith f (cdr x) (cdr y))))
          (error 'zipWith "lists have different lengths")))))