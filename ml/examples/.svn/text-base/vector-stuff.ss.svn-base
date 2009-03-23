(module vector-stuff mzscheme
  (provide (all-defined))
  ; int * 'a -> 'a
  (define vector1 (vector 3 (lambda (x) x)))  
  ; int * lump
  (define vector2 (vector 1 'b))
  ; int * (int * lump) * bool
  (define vector3 (vector 5 vector1 #f))
  ; ('a * 'b * 'c -> 'a) * ('a * 'b * 'c -> 'b) * ('a * 'b * 'c -> 'c)
  (define vector4 (vector (lambda (v) (vector-ref v 0)) (lambda (v) (vector-ref v 1)) (lambda (v) (vector-ref v 2)))) 
  (define vectora (vector 3 #f))
  (define vectorb (vector #f 3))
  (define inta 34)
)
