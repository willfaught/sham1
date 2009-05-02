(module Primitives scheme
  (provide Char/haskell/c
           Float/haskell/c
           Int/haskell/c
           List#/haskell/c
           Tuple#/haskell/c
           (rename-out (constructor/Char-value variable/charValue#)
                       (constructor/Float-value variable/floatValue#)
                       (constructor/Int-value variable/intValue#)
                       (make-constructor/Char variable/Char)
                       (make-constructor/Float variable/Float)
                       (make-constructor/Int variable/Int)
                       (make-constructor/Tuple# variable/Tuple#)))
  
  )