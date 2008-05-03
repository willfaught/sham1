#reader (lib "haskell.ss" "haskell")

module Prelude where {

error = :scheme [Char] -> a "haskell:error";

trace = :scheme a -> b -> b "haskell:trace";

(+) = :scheme Int -> Int -> Int "haskell:int-add";

(-) = :scheme Int -> Int -> Int "haskell:int-subtract";

(*) = :scheme Int -> Int -> Int "haskell:int-multiply";

(/) = :scheme Int -> Int -> Int "haskell:int-divide";

(==) = :scheme Int -> Int -> Bool "haskell:int-equal";

(/=) = :scheme Int -> Int -> Bool "haskell:int-not-equal";

head = :scheme [a] -> a "haskell:list-head";

tail = :scheme [a] -> [a] "haskell:list-tail";

null = :scheme [a] -> Bool "haskell:list-null";

fst = :scheme (a, b) -> a "haskell:tuple-first";

snd = :scheme (a, b) -> b "haskell:tuple-second";

true = :scheme Bool "haskell:boolean-true";

false = :scheme Bool "haskell:boolean-false";

(&&) = :scheme Bool -> Bool -> Bool "haskell:boolean-and";

(||) = :scheme Bool -> Bool -> Bool "haskell:boolean-or";

not = :scheme Bool -> Bool "haskell:boolean-not"

}