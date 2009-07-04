#reader (lib "Reader.ss" "sham" "haskell")

module Haskell.Data.Maybe (Nothing, Just, fromJust, maybe, fromMaybe, listToMaybe, maybeToList, catMaybes, mapMaybe) where {

data Maybe a = Nothing | Just { fromJust :: a };

maybe d f v = if isNothing v then d else f (fromJust v);

fromMaybe d v = if isNothing v then d else fromJust v;

listToMaybe x = if Haskell.Prelude.null x then Nothing else Haskell.Prelude.head x;

maybeToList x = if isNothing x then [] else [x];

catMaybes x = if Haskell.Prelude.null x then [] else let { h = Haskell.Prelude.head x ; rest = catMaybes (Haskell.Prelude.tail x) } in if isNothing h then rest else (Haskell.Prelude.:) h rest;

map f x = if Haskell.Prelude.null x then [] else (Haskell.Prelude.:) (f (Haskell.Prelude.head x)) (map f (Haskell.Prelude.tail x));

mapMaybe f x = if Haskell.Prelude.null x then [] else let { h = f (Haskell.Prelude.head x) ; rest = mapMaybe f (Haskell.Prelude.tail x) } in if isNothing h then rest else (Haskell.Prelude.:) (fromJust h) rest

}