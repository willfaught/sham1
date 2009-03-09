#reader (lib "Reader.ss" "sham" "haskell")

module CompilationTest1 (ap1, ap2) where {

ap1 = (\x -> x) 1;

ap2 = (\x y -> x) 1;

ap3 = (\x y -> y) 1;

ap4 = (\x y -> x) 1 2;

ap5 = (\x y -> y) 1 2;

ch1 = 'a';

fl1 = 1.2;

fu1 = \x -> x;

fu2 = \x y -> x;

fu3 = \x y -> y;

id1 = Prelude.fst;

id2 = Prelude.head;

id3 = Prelude.isFalse;

id4 = Prelude.isTrue;

id5 = Prelude.null;

id6 = Prelude.snd;

id7 = Prelude.tail;

id8 = Prelude.False;

id9 = Prelude.True;

id10 = (Prelude.:);

if1 = if Prelude.True then 1 else 2;

if2 = if Prelude.False then 1 else 2;

in1 = 1;

le1 = let { i = 1 } in 2;

le2 = let { i = 1 } in i;

le3 = let { i = 1 ; j = 2 } in 3;

le4 = let { i = 1 ; j = i } in j;

le5 = let { i = j ; j = 1 } in i;

le6 = let { i x = 1 } in i;

le7 = let { i x = x } in i;

le8 = let { i x y = x } in i;

li1 = [];

li2 = [1];

li3 = [1, 2];

tc1 = (,);

tc2 = (,,);

tu1 = (1, 2.3);

tu2 = (1, 2.3, 'a');

un1 = ()

}