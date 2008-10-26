#reader (lib "Reader.ss" "sham" "haskell")

module Test where {

-- applications

ap1 = (\x -> 1) 'a';
ap2 = (\x -> x) [];
ap3 = (\x -> x) (\x -> x);
ap5 = (\x y -> 1) 2 3;
ap6 = (\x -> \y -> y) 'a' (1.2, 3);

-- characters

ch1 = 'a';

-- booleans

bo1 = False;
bo2 = True;

-- datas

data Tree = Branch { left :: Tree, right :: Tree } | Leaf { leaf :: Fruit } | Ha;
data Fruit = Apple { foo :: Tree } | Orange { bar :: Tree };
data A = A;

-- floats

fl1 = 1.2;

-- functions

fu1 = \x -> 1;
fu2 = \x -> x;
fu3 = \x y -> x;
fu4 = \x y -> y;

-- ifs

if1 = if True then 1 else 2;
if2 = if False then 'a' else 'b';
if3 = if (\x -> x) False then 1 else 2;
if4 = if if True then True else False then 'a' else 'b';

-- integers

in1 = 1;

-- lets

le1 = let { a = 1 } in 1;
le2 = let { a = 1 } in a;
le3 = let { a = a } in a;
le4 = let { a = 1 ; b = a } in b;
le5 = let { a = b; b = 1 } in a;
le6 = let { a = b ; b = a } in a;
le7 = let { a x = x } in a 1;
le8 = let { a x = x } in a (a 1);
le9 = let { a x = x ; b = a 2 ; c = a 3 } in a;
le10 = let { a = 1 } in let { b = a } in b;
le11 = let { a x = x } in let { b = a 2 } in b;
le12 = let { a x = x } in let { b = a 2 ; c = a 'b' } in a;

-- lists

li1 = [];
li2 = ['a'];
li3 = [3.4];
li4 = [5];
li5 = [[]];
li6 = [[], []];
li7 = [['a']];
li8 = [['a'], ['b']];
li9 = [1, 2, 3];
li10 = "foo";
li11 = [(1, 2)];
li12 = [('a', 'b', 'c'), ('d', 'e', 'f')];

-- misc

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
fib = (:) 0 ((:) 1 (zipWith (+) fib (tail fib)));

-- tuples

tu1 = ('a', 'b');
tu2 = (1.2, 3.4, 5.6);
tu3 = ('a', 1.2);
tu4 = (3.4, 5, 'b');
tu5 = (('a', 1.2), (3.4, 5, 'b'), 6);
tu6 = ([], []);
tu7 = ([1, 2, 3], ['a', 'b']);

-- primitives

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

-- tuplecons

tc1 = (,);
tc2 = (,,);
tc3 = (,) 1;
tc4 = (,) 'a' 2;
tc5 = (,) (,) (,);

-- units

un1 = ()

}