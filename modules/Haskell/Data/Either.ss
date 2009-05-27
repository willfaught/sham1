#reader (lib "Reader.ss" "sham" "haskell")

module Either (Left, Right, left, right, either, lefts, rights, partitionEithers) where {

data Either a b = Left { left :: a } | Right { right :: b };

either f g x = if isLeft x then f (left x) else g (right x);

lefts x = if Haskell.Prelude.null x then [] else let { h = Haskell.Prelude.head x; rest = lefts (Haskell.Prelude.tail x) } in if isLeft h then (Haskell.Prelude.:) h rest else rest;

rights x = if Haskell.Prelude.null x then [] else let { h = Haskell.Prelude.head x; rest = rights (Haskell.Prelude.tail x) } in if isRight h then (Haskell.Prelude.:) h rest else rest;

filter p x = let { h = Haskell.Prelude.head x ; t = Haskell.Prelude.tail x ; rest = filter p t } in if p h then (Haskell.Prelude.:) h rest else rest;

partitionEithers x = (filter (\y -> isLeft y) x, filter (\y -> isRight y) x)

}