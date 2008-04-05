#reader (lib "haskell-reader.ss" "hs")

module Simple where {

a = [1, 2, 3];

--b = [4, 5, 6];

--zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

--c = zipWith (+) a b;

--(!!) = 4

elem x n = let { elem x m = if (==) m n then head x else elem (tail x) ((+) m 1) } in elem x 0

}