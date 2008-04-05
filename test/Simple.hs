#reader (lib "haskell-reader.ss" "hs")

module Simple where {

zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

x !! n = if (==) n 0 then head x else (!!) (tail x) ((-) n 1);

fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)))

}