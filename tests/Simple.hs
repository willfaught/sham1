#reader (lib "haskell-reader.ss" "hs")

module Simple where {

--zip x y = if null x then [] else (:) (head x, head y) (zip (tail x) (tail y));

--zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

--x !! n = if (==) n 0 then head x else (!!) (tail x) ((-) n 1);

--x ++ y = if null x then y else (:) (head x) ((++) (tail x) y);

--fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)))

--a = (+) :: Int -> Int -> Int

-- type tests

char1 = 'a';
char2 = 'A';
float1 = 1.2;
int1 = 1;
list1 = [];
list2 = ['a'];
list3 = [3.4];
list4 = [5];
list5 = [('a', 6.7, 8), ('b', 9.0, 1)];
list6 = "test";
list7 = [1, 2, 3];
tuple1 = ('a', 1.2, 3, [4], ('b', 4));
let1 = let { a x = x } in a 3

}