#reader (lib "Reader.ss" "sham" "haskell")

module List ((++), length, map, foldl, foldr, filter, (!!)) where {

x ++ y = if Haskell.Prelude.null x then y else (Haskell.Prelude.:) (Haskell.Prelude.head x) ((++) (Haskell.Prelude.tail x) y);

length x = if Haskell.Prelude.null x then 0 else (Haskell.Prelude.+) 1 (length (Haskell.Prelude.tail x));

map f x = if Haskell.Prelude.null x then [] else (Haskell.Prelude.:) (f (Haskell.Prelude.head x)) (Haskell.Prelude.tail x);

foldl f i x = if Haskell.Prelude.null x then i else foldl f (f i (Haskell.Prelude.head x)) (Haskell.Prelude.tail x);

foldr f i x = if Haskell.Prelude.null x then i else f (Haskell.Prelude.head x) (foldr f i (Haskell.Prelude.tail x));

filter p x = let { h = Haskell.Prelude.head x ; t = Haskell.Prelude.tail x ; rest = filter p t } in if p h then (Haskell.Prelude.:) h rest else rest;

x !! n = 0

}