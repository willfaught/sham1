#reader (lib "reader.ss" "haskell")

module Test where {

import "/Users/wfaught/haskell/lib/empty.ss";

data Tree = Branch { left :: Tree, right :: Tree } | Leaf { leaf :: Fruit } | Ha;

data Fruit = Apple { foo :: Tree } | Orange { bar :: Tree };

data A = A;

-- character tests

char1 = 'a';

-- float tests

float1 = 1.2;

-- integer tests

int1 = 3;

-- boolean tests

--bool1 = true;
--bool2 = false;

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
list10 = "foo";
list11 = [(1, 2)];
list12 = [('a', 'b', 'c'), ('d', 'e', 'f')];

-- tuple tests

tuple1 = ('a', 'b');
tuple2 = (1.2, 3.4, 5.6);
tuple3 = ('a', 1.2);
tuple4 = (3.4, 5, 'b');
tuple5 = (('a', 1.2), (3.4, 5, 'b'), 6);
tuple6 = ([], []);
tuple7 = ([1, 2, 3], ['a', 'b']);

-- function tests

fun1 = \x -> 1;
fun2 = \x -> x;
fun3 = \x y -> x;
fun4 = \x y -> y;

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
let12 = let { a x = x } in let { b = a 2 ; c = a 'b' } in a;

-- prelude tests

(+) = :scheme Int -> Int -> Int "primitive:number-add";

(-) = :scheme Int -> Int -> Int "primitive:number-subtract";

(*) = :scheme Int -> Int -> Int "primitive:number-multiply";

(/) = :scheme Int -> Int -> Int "primitive:number-divide";

(==) = :scheme Int -> Int -> Bool "primitive:equal";

(/=) = :scheme Int -> Int -> Bool "primitive:not-equal";

pr1 = let { i = [(+) ((+) 1 2) (((+) 1) 2), (-) ((-) 1 2) (((-) 1) 2), (*) ((*) 1 2) (((*) 1) 2), (/) ((/) 1 2) (((/) 1) 2)] } in i;
pr2 = let { i = (:) 'f' ((:) 'o' "o") } in i;
pr3 = let { i = (:) (head ['a']) [] } in i;
pr4 = let { i = (:) 'a' (tail ['b']) } in i;
pr5 = let { i = (:) (fst ('a', 1)) [] } in i;
pr6 = let { i = (:) (snd (1, 'a')) [] } in i;

-- nested tests

x && y = if isFalse x then False else y;

x || y = if isTrue x then True else y;

not x = if isTrue x then False else True;

map f x = if null x then [] else (:) (f (head x)) (map f (tail x));

filter p x = if null x then [] else let { h = head x ; t = tail x } in if p h then (:) h (filter p t) else filter p t;

foldl f z x = let { fold z x = if null x then z else fold (f z (head x)) (tail x) } in fold z x;

and x = if null x then True else (&&) (head x) (and (tail x));

or x = if null x then False else (||) (head x) (or (tail x));

foldr f z x = let { fold x = if null x then z else f (head x) (fold (tail x)) } in fold x;

length x = if null x then 0 else (+) 1 (length (tail x));

flip f x y = f y x;

reverse x = let { rev x a = if null x then a else rev (tail x) ((:) (head x) a) } in rev x [];

(.) f g x = f (g x);

zip x y = if null x then [] else (:) (head x, head y) (zip (tail x) (tail y));

zipWith f x y = if null x then [] else (:) (f (head x) (head y)) (zipWith f (tail x) (tail y));

x !! n = if (==) n 0 then head x else (!!) (tail x) ((-) n 1);

x ++ y = if null x then y else (:) (head x) ((++) (tail x) y);

fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)))

}