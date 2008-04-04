#reader (lib "haskell-reader.ss" "hs")

module Simple where {

--foo x y = (+) x y;

--bar = \x -> \y -> (:) x y;

--baz x = case x of { y -> 1 };
                      
--boz = let { x = 3 ; y a = a ; z a b = (-) a b } in z (y x) (y x)
  
boz = let { x y z = (-) y z } in x 3 4
}