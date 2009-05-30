#reader (lib "Reader.ss" "sham" "haskell")

module Either (Left, Right, left, right, either, lefts, rights, partitionEithers) where {

import haskell Haskell.Data.List (filter :: (a -> Bool) -> [a] -> [a]);

data Either a b = Left { left :: a } | Right { right :: b };

either f g x = if isLeft x then f (left x) else g (right x);

lefts x = if Haskell.Prelude.null x then [] else let { h = Haskell.Prelude.head x; rest = lefts (Haskell.Prelude.tail x) } in if isLeft h then (Haskell.Prelude.:) h rest else rest;

rights x = if Haskell.Prelude.null x then [] else let { h = Haskell.Prelude.head x; rest = rights (Haskell.Prelude.tail x) } in if isRight h then (Haskell.Prelude.:) h rest else rest;

partitionEithers x = (Haskell.Data.List.filter (\y -> isLeft y) x, Haskell.Data.List.filter (\y -> isRight y) x)

}