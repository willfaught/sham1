#reader (lib "Reader.ss" "sham" "haskell")

module List ((++), map, filter) where {

x ++ y = if Haskell.Prelude.null x then y else (Haskell.Prelude.:) (Haskell.Prelude.head x) ((++) (Haskell.Prelude.tail x) y);

map f x = if Haskell.Prelude.null x then [] else (Haskell.Prelude.:) (f (Haskell.Prelude.head x)) (Haskell.Prelude.tail x);

filter p x = let { h = Haskell.Prelude.head x ; t = Haskell.Prelude.tail x ; rest = filter p t } in if p h then (Haskell.Prelude.:) h rest else rest

}