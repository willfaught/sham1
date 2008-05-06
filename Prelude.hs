#reader (lib "haskell.ss" "haskell")

module Prelude where {

--error = :scheme [Char] -> a "prelude:error";

--trace = :scheme a -> b -> b "prelude:trace";

(+) = :scheme Int -> Int -> Int "prelude:int-add";

(-) = :scheme Int -> Int -> Int "prelude:int-subtract";

(*) = :scheme Int -> Int -> Int "prelude:int-multiply";

(/) = :scheme Int -> Int -> Int "prelude:int-divide";

--(==) = :scheme Int -> Int -> Bool "prelude:int-equal";

--(/=) = :scheme Int -> Int -> Bool "prelude:int-not-equal";

}