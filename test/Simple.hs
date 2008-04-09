#reader (lib "haskell-reader.ss" "hs")

module Simple where {

--zip x y = if null x then [] else (:) (head x, head y) (zip (tail x) (tail y));

--zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

--x !! n = if (==) n 0 then head x else (!!) (tail x) ((-) n 1);

--x ++ y = if null x then y else (:) (head x) ((++) (tail x) y);

--fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)))

a = let { x = 1 :: Int } in 1

}