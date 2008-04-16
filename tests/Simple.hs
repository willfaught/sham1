#reader (lib "haskell-reader.ss" "hs")

module Simple where {

--zip x y = if null x then [] else (:) (head x, head y) (zip (tail x) (tail y));

--zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

--x !! n = if (==) n 0 then head x else (!!) (tail x) ((-) n 1);

--x ++ y = if null x then y else (:) (head x) ((++) (tail x) y);

--fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)))

--a = (+) :: Int -> Int -> Int

-- character tests

char1 = 'a';

-- float tests

float1 = 1.2;

-- integer tests

int1 = 3;

-- boolean tests

bool1 = True;
bool2 = False;

-- list tests

list1 = [];
list2 = ['a'];
list3 = [3.4];
list4 = [5];
list5 = [[]];
list6 = [[], []];
list7 = [['a']];
list8 = [['a'], ['b']];
list9 = [1, 2, 3];
list10 = "test";

-- tuple tests

tuple1 = ('a', 'b');
tuple2 = (1.2, 3.4, 5.6);
tuple3 = ('a', 1.2);
tuple4 = (3.4, 5, 'b');
tuple5 = (('a', 1.2), (3.4, 5, 'b'), 6);

-- nested tests

nested1 = [(1, 2)];
nested2 = [('a', 'b', 'c'), ('d', 'e', 'f')];
nested3 = ([], []);
nested4 = ([1, 2, 3], ['a', 'b']);

-- function tests

fun1 = \x -> 1;
fun2 = \x -> x;
fun3 = \x y -> x;
fun4 = \x y -> y;
fun5 = \x -> \y -> y;

-- application tests

app1 = (\x -> 1) 'a';
app2 = (\x -> x) [];
app3 = (\x -> x) (\x -> x);
app5 = (\x y -> 1) 2 3;
app6 = (\x -> \y -> y) 'a' (1.2, 3);

-- tuplecon tests

tupcon1 = (,);
tupcon2 = (,,);
tupcon3 = (,) 1;
tupcon4 = (,) 'a' 2;
tupcon5 = (,) (,) (,);

-- if tests

if1 = if True then 1 else 2;
if2 = if False then 'a' else 'b';
if3 = if (\x -> x) False then 1 else 2;
if4 = if if True then True else False then 'a' else 'b';

-- let tests

let1 = let { a = 1 } in 1;
let2 = let { a = 1 } in a;
let3 = let { a = a } in a;
let4 = let { a = 1 ; b = a } in b;
let5 = let { a = b; b = 1 } in a;
let6 = let { a = b ; b = a } in a;
let7 = let { a x = x } in a 1;
let8 = let { a x = x } in a (a 1);
let9 = let { a x = x ; b = a 2 ; c = a 3 } in a;
let10 = let { a = 1 } in let { b = a } in b;
let11 = let { a x = x } in let { b = a 2 } in b;
let12 = let { a x = x } in let { b = a 2 ; c = a 'b' } in a

-- prelude tests



}